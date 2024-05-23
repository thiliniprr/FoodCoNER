# ---- Packages and Initialization ----
oldw <- getOption("warn")  # Temporarily disable warnings
options(warn = -1)

required_packages <- c(
  "stringr", "RISmed", "XML", "stopwords", "stringi", "NLP", "tm", "udpipe",
  "textstem", "spacyr", "openNLP", "dplyr", "crfsuite"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    #install.packages(pkg)
    stop(paste("Package", pkg, "not found. Please install it."), call. = FALSE)
  }
}

reticulate::use_python("C:/Users/thili/anaconda3/python.exe")  # Set Python path
spacy_initialize(model = "en_core_web_sm")

options(warn = oldw)    # Restore warning settings

# ---- Data Loading ----
stopwords <- as.character(read.delim(file.path(getwd(), "stopewords.txt"), sep = '\n')[, 1])

### Text Pre-Processing Steps ####################################################

#Function to clean unwanted characters
clean_sentence = function(x) {
  x=as.character(x)
  x=gsub("[^[:alnum:]\\s\\.\\?\\!]", replacement = " ", tolower(x))
  x=gsub(' +',' ',x)
  return(unlist(trimws(x)))
}

clean = function(x) {
  x=as.character(x)
  x=gsub("[\\!\\?]]", replacement = ".", tolower(x))
  x=gsub("[^[:alnum:]\\-\\.\\,\\']", replacement = " ", tolower(x))
  x=gsub(' +',' ',x)
  return(unlist(trimws(x)))
}

clean_all = function(x) {
  x=as.character(x)
  x=gsub("[^[:alpha:]]", replacement = " ", tolower(x))
  x=gsub(' +',' ',x)
  return(unlist(trimws(x)))
}

whitespace_remove = function(x) {
  x=as.character(x)
  x=gsub(' +',' ',x)

  return(unlist(trimws(x)))
}

# Stem the tokenized words
stem_words =function(x){
  unlist(lapply(x, lemmatize_words))
}

stopwords_remove = function(x){
  x<-stri_remove_empty(removeWords(x,stopwords("en")))
  x<-stri_remove_empty(removeWords(x,stopwrds))
  return(whitespace_remove(x))
}

custom_clean=function(x){
  x=gsub("[^[:alnum:]\\'\\,\\(\\)\\.\\-]", replacement = " ", tolower(x))
  x=gsub(' +',' ',x)
  return(unlist(trimws(x)))
  return(x)
}

get_pos_tag=function(x, pos_type){
  x<-as.String(x)
  word_anno <- annotate(x, list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))
  pos_tag_annotator <- annotate(x, Maxent_POS_Tag_Annotator(), word_anno)
  words <- subset(pos_tag_annotator, type == "word")
  tags <- sapply(words$features, '[[', "POS")
  pos_type_ind <- grep(pos_type, tags)
  tagged <- sprintf("%s/%s", x[words][pos_type_ind], tags[pos_type_ind])
  tagged<-gsub("/(.*)","",tagged)
}

#
#
## Function to get noun phrses from a give text
#
#
get_nounphrases = function(x){
  if(length(x)!=0){
    x<-as.String(x)
    word_anno <- annotate(x, list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))
    pos_tag_annotator <- annotate(x, Maxent_POS_Tag_Annotator(), word_anno)

    ## Seperate tokenized sentences and words
    words <- subset(pos_tag_annotator, type == "word")
    sent <- subset(pos_tag_annotator, type == "sentence")

    ## Get the word indexes corresponding to each sentence
    const <- sapply(sent$features, '[[', "constituents")

    ## Get POS tags of each sentence
    tags <- sapply(words$features, '[[', "POS")

    ## Create a data frame with tokenized words, their pos tags, sentence id
    annotated<-as.data.frame(words)
    annotated$token=x[words]
    annotated$xpos=tags
    offset <- annotated[1,"id"]-1 # offset deals with the word token id issue related to openNL POS annotator id
    annotated$id<-annotated$id-offset
    annotated1 <- annotated[,c(1,6,7)]
    annotated1$sent_id <- 0
    b=1
    for (it in const) {
      mx <- max(as.vector(it))-offset
      mn <- min(as.vector(it))-offset
      annotated1[mn:mx,"sent_id"]<-b
      b=b+1
    }

    options(warn = -1)
    annotated1<-annotated1[!(annotated1$xpos=="."),]
    ## Uses udpipe to extract noun phrases
    ## Find noun phrases with the following regular expression: (A|N)+N(P+D*(A|N)*N)*
    annotated1$phrase_tag <- as_phrasemachine(annotated1$xpos, type = "penn-treebank")
    nounphrases <- keywords_phrases(annotated1$phrase_tag, term = annotated1$token,
                                    pattern = "(A|N)+N(P+D*(A|N)*N)*", is_regex = TRUE,
                                    ngram_max = 2,
                                    detailed = TRUE)
    options(warn = oldw)

    return(nounphrases)
  }
}


extract_nounphrases_spacyr=function(x){
  if(nchar(x)>3){
    np<-spacy_extract_nounphrases(x)
    np<-np$text
    # for(t in 1:length(np)){
    #   verbs<-extract_POS_spacyr(as.character(np[t]),FALSE,"VERB")
    #   if(length(verbs)>0){
    #     for(i in 1:length(verbs))
    #       np[t]<-trimws(gsub(verbs[i],"",np[t]))
    #   }
    # }

    return(np)
  }
  return(NA)
}

extract_POS_spacyr=function(x,with_lemma){
  pos<-spacy_parse(as.character(tolower(x)), tag = TRUE, entity = FALSE, lemma = with_lemma)
  return(pos)
}

extract_POS_spacyr=function(x,with_lemma,tag_type){
  x<-unlist(str_split(x, " "))
  tags<-spacy_parse(as.character(tolower(x)), tag = TRUE, entity = FALSE,lemma = with_lemma)
  tags<-tags[tags$pos==tag_type,"token"]
  return(tags)
}

## Search Pubmed fo pubmedids & use pubmineR to download Abstracts#################

get_data_by_Query = function(my_query, retsize,mindate,maxdate){
  my_query<-as.character("((diet AND disease) AND association)")

  require(RISmed)

  tryCatch({

    if(nchar(my_query)>0){
      search_query <- EUtilsSummary(my_query, db='pubmed', retmax=2000, mindate=2010,maxdate=2021)
      #summary(search_query)

      # see the ids of our returned query
      #QueryId(search_query)

      # get actual data from PubMed
      records<- EUtilsGet(search_query)
      #class(records)

      # store it
      pubmed_data <- data.frame('PMID'=ArticleId(records),'Title'=ArticleTitle(records),'Abstract'=AbstractText(records), 'Journal'=)
      #head(pubmed_data,1)

      pubmed_data$Abstract <- whitespace_remove(as.character(pubmed_data$Abstract))
      pubmed_data$Abstract[nchar(pubmed_data$Abstract)==0]=NA
      pubmed_data<- pubmed_data[(!is.na(pubmed_data$Title) & !is.na(pubmed_data$Abstract)),]
      pubmed_data$Abstract<-gsub("Label=(.*?):","",pubmed_data$Abstract)

      #save(pubmed_data, file = "pubmed_diabetes_diet.RData")
      return (pubmed_data)
    }
  }, error = function(e){
    return (e)
  })
  return (-1)
}

#
#
#
