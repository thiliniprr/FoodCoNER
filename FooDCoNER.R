# Load required packages and functions
if (!requireNamespace("crfsuite", quietly = TRUE)) {
  install.packages("crfsuite")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(crfsuite)
library(stringr)


# Load data
source("utils.R")
load(file = "dietaryDictionary.Rdata")
dietaryDictionary <- trimws(newdietdict)
load(file = "foodbase_features_for_CRF.Rdata")


# Define training function
train.model <- function() {
  # Check if output data is available and load if needed
  if (!exists("output")) {
    load(file = "foodbase_features_for_CRF.Rdata")
  }
  
  crf_train <- output
  
  tryCatch({
    # NER model training
    model_ner <- crf(
      y = crf_train$isNER,
      x = crf_train[, c("isNN", "nounphrase", "inDict", "dep_rel", "token",
                       "token_previous", "token_next", "pos", "pos_previous", "pos_next")],
      group = crf_train$doc_id,
      method = "lbfgs",
      file = "dietTaggerBin.crfsuite",
      options = list(max_iterations = 250, feature.minfreq = 2, c1 = 0.1, c2 = 0.1)
    )
    
    # IOB model training
    model_iob <- crf(
      y = crf_train$Label,
      x = crf_train[, c("isNN", "nounphrase", "inDict", "dep_rel", "token",
                       "token_previous", "token_next", "pos", "pos_previous", "pos_next")], 
      group = crf_train$doc_id,
      method = "lbfgs",
      file = "dietTaggerIOB.crfsuite",
      options = list(max_iterations = 250, feature.minfreq = 2, c1 = 0.1, c2 = 0.1)
    )
    
    # Return a list of trained model filenames
    invisible(list("dietTaggerIOB.crfsuite", "dietTaggerBin.crfsuite"))
  }, error = function(e) {
    # Handle errors during model training
    cat("Error occurred during model training:", e$message, "\n")
    invisible(NULL)
  })
}

food.annotator.crf=function(text){

  if(length(text)>=3 & typeof(text[,2])=="character"
     & typeof(text[,3])=="character"){
    text[,2]<-paste(text[,2],text[,3], sep=" ")
    text<-text[,-3]
  }
  else if(length(text)<2){
    return(NA)
  }

  taglist<-data.frame(PMID=as.integer(), annotation = as.character())#, position = as.character())
  sucess=FALSE

  if((!exists("modelisNER")) || (!exists("modelisNER"))){
    tryCatch({
      modelIOB<-as.crf(file = "dietTaggerIOB.crfsuite")
      modelisNER<-as.crf(file = "dietTaggerBin.crfsuite")
      sucess=TRUE
      },
      warning = function(w) {
      message<-w
    }, error = function(e) {
      modelarray<-unlist(train.model())
      modelIOB<-as.crf(file = "dietTaggerIOB.crfsuite")
      modelisNER<-as.crf(file = "dietTaggerBin.crfsuite")
      sucess=TRUE
    })

    if(!sucess)
      return(message)
  }

  crf_test<-preprocess(text)

  # Evaluate the model using test set
  scores <- predict(modelisNER,
                    newdata = crf_test[, c("isNN","nounphrase","inDict","dep_rel",#"dependency",
                                           "token", "token_previous", "token_next",#"token_previous_prev", "token_next_next",# "token_prev_prev_prev","token_next_next_next",
                                           "pos", "pos_previous", "pos_next","pos_previous_prev","pos_next_next"#,"pos_next_next_next","pos_prev_prev_prev"
                    )],
                    group = crf_test$doc_id)

  scoresIOB <- predict(modelIOB,
                    newdata = crf_test[, c("isNN","nounphrase","inDict","dep_rel",#"dependency",
                                           "token", "token_previous", "token_next",#"token_previous_prev", "token_next_next",# "token_prev_prev_prev","token_next_next_next",
                                           "pos", "pos_previous", "pos_next","pos_previous_prev","pos_next_next"#,"pos_next_next_next","pos_prev_prev_prev"
                    )],
                    group = crf_test$doc_id)

  crf_test$isNER <- scores$label
  crf_test$labels <- scoresIOB$label
  crf_test1<-crf_test

  for(i in 1:length(crf_test[,1])){
    if(crf_test$labels[i]=="B" && crf_test$isNER[i]=="1"){
      tag=as.character(crf_test$token[i])

      while(crf_test$labels[i+1]=="I" && crf_test$isNER[i+1]=="1"){
        i<-i+1
        tag<-paste(tag,as.character(crf_test$token[i]), sep = " ")
      }
      tag<-clean_all(tag)
      # starts<-str_locate(text[text$PMID %in% crf_test[i-1,1],2],trimws(tag))
      # taglist <- rbind(taglist, data.frame(PMID=crf_test[i-1,1],annotation=tag, position=paste(as.character(starts[[1]]),nchar(tag))))

      #starts<-str_locate_all(text[text$PMID %in% crf_test[i-1,1],2],trimws(tag))[[1]][,"start"]

      #if(length(starts)>0){
        #for(t in 1:length(starts)){
          taglist <- rbind(taglist, data.frame(PMID=crf_test[i-1,1],annotation=tag))# position=paste(starts[t],nchar(tag))))
        #}
      #}
    }
  }

  #return list of words where isNER==1 -> convert to IBO format and send phrases as well
  return(taglist)
  #return(crf_test)
}


preprocess=function(text){
  texts <- as.character(text[,2])
  names(texts) <- as.character(text[,1])
  output<-spacy_parse(trimws(texts),pos=TRUE,lemma=FALSE,tag=TRUE,entity = FALSE,
                      dependency = TRUE, nounphrase = TRUE)

  require(data.table)
  require(crfsuite)
  #require(caret)

  output <- as.data.table(output)
  output <- output[, pos_previous   := shift(pos, n = 1, type = "lag"), by = list(doc_id)]
  output <- output[, pos_next       := shift(pos, n = 1, type = "lead"), by = list(doc_id)]
  output <- output[, token_previous := shift(token, n = 1, type = "lag"), by = list(doc_id)]
  output <- output[, token_next     := shift(token, n = 1, type = "lead"), by = list(doc_id)]
  output <- output[, pos_previous_prev   := shift(pos, n = 2, type = "lag"), by = list(doc_id)]
  output <- output[, pos_next_next       := shift(pos, n = 2, type = "lead"), by = list(doc_id)]

  output <- output[, pos_previous   := txt_sprintf("pos[w-1]=%s", pos_previous), by = list(doc_id)]
  output <- output[, pos_next       := txt_sprintf("pos[w+1]=%s", pos_next), by = list(doc_id)]
  output <- output[, token_previous := txt_sprintf("token[w-1]=%s", token_previous), by = list(doc_id)]
  output <- output[, token_next     := txt_sprintf("token[w+1]=%s", token_next), by = list(doc_id)]
  output <- output[, pos_previous_prev   := txt_sprintf("pos[w-2]=%s", pos_previous_prev), by = list(doc_id)]
  output <- output[, pos_next_next       := txt_sprintf("pos[w+2]=%s", pos_next_next), by = list(doc_id)]


  output$isNN <- startsWith(output$tag, "NN")

  output$inDict<-"0"

  output$inDict[(which(tolower(output$token) %in% dietaryDictionary))]<-"1"
  output$inDict[(which(stem_words(tolower(output$token)) %in% dietaryDictionary))]<-"1"
  #output$inDict[(which(tolower(output$token) %in% diseasesDictionary$MENTIONS))]<-"1"
  #output$inDict[(which(stem_words(tolower(output$token)) %in% diseasesDictionary$MENTIONS))]<-"1"

  # for (i in 1:length(output$doc_id)){
  #   if(nchar(output$token[i])>2 && !(output$token[i] %in% stopwrds)){
  #     noun<-clean_all(output$token[i])
  #     stem<-clean_all(stem_words(noun))
  #
  #     if(length(grep(paste0("\\b",noun,"\\b"),dietaryDictionary))>0
  #        || length(grep(paste0("\\b",stem,"\\b"),dietaryDictionary))>0
  #        #|| length(grep(paste0("\\b",noun,"\\b"),cc$MENTIONS))>0
  #        #|| length(grep(paste0("\\b",stem,"\\b"),cc$MENTIONS))>0
  #        ){
  #       output$inDict[i]<-"1"
  #     }
  #   }
  # }


  output <- as.data.frame(output)
  output$nounphrase[nchar(output$nounphrase)<2]<-"O"
  output$nounphrase[output$nounphrase=="beg" & output$pos == "DET"]<-"O"
  output$nounphrase[output$nounphrase=="beg"]<-"B"
  output$nounphrase[output$nounphrase=="mid"]<-"I"
  output$nounphrase[output$nounphrase=="end_root"]<-"I"
  output$nounphrase[output$nounphrase=="beg_root"]<-"B"

  for(np in 2:length(output$nounphrase)){
    if(output$nounphrase[np]=="I" & output$nounphrase[np-1]=="O")
      output$nounphrase[np]<-"B"
  }

  return(output)
}

processLabels = function(fulltexts,annotation, output1){

  for (i in 1:length(fulltexts[,1])){
    annota_all<-unique(annotation[annotation[,1]==fulltexts[i,1],2])
    annota_one<-annota_all[lapply(str_split(annota_all,boundary("word")),length)<2]

    if(length(annota_one)>0)
      annota_all<-setdiff(annota_all,annota_one)

    pos_loc <-data.frame(loc=which(output1[,1]==fulltexts[i,1]), pos=output1[output1[,1]==fulltexts[i,1],4])
    pos_loc <-rbind(pos_loc, data.frame(loc=145, pos="-"))
    pos_loc <- pos_loc[nchar(as.character(pos_loc[,2]))>1 | as.character(pos_loc[,2])=="-",]

    for(j in 1:length(pos_loc[,1])){

      if(!grepl("\\d",pos_loc[j,2])){

        #For single word NER
        tag1<-grep(paste0("\\b",pos_loc[j,2],"\\b"),annota_one)

        if(length(tag1)>0){
          output1$isNER[pos_loc[j,1]]<-1
        }

        #For compound NER
        tag<-grep(paste0("\\b",pos_loc[j,2],"\\b"),annota_all)

        if(length(tag)>0){
          for(t in 1:length(tag)){
            wrd_count<-length(unlist(str_split(annota_all[tag[t]],boundary("word"))))-1

            if(grepl("-",annota_all[tag[t]])){
              wrd_count=wrd_count+length(grep("-",annota_all[tag[t]]))
            }

            comp_wrd<-trimws(paste(as.character(pos_loc[c(j:(j+wrd_count)),2]),collapse = " "))

            if(grepl("-",comp_wrd)){
              comp_wrd=trimws(gsub(" - ","-",comp_wrd))
            }

            if(comp_wrd==annota_all[tag[t]]){
              output1$isNER[pos_loc[c(j:(j+wrd_count)),1]]<-1
              #print(paste(comp_wrd,annota_all[tag[t]],wrd_count,sep="/"))
              j=j+wrd_count
            }
          }
        }
      }
    }
  }
  return(output1)
}
