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
    # Train CRF model for Named Entity Recognition (NER)
    model_ner <- crf(
      y = crf_train$isNER,
      x = crf_train[, c("isNN", "nounphrase", "inDict", "dep_rel", "token",
                       "token_previous", "token_next", "pos", "pos_previous", "pos_next")],
      group = crf_train$doc_id,
      method = "lbfgs",
      file = "dietTaggerBin.crfsuite",
      options = list(max_iterations = 250, feature.minfreq = 2, c1 = 0.1, c2 = 0.1)
    )
    
    # Train CRF model for Inside-Outside-Beginning (IOB) tagging
    model_iob <- crf(
      y = crf_train$Label,
      x = crf_train[, c("isNN", "nounphrase", "inDict", "dep_rel", "token",
                       "token_previous", "token_next", "pos", "pos_previous", "pos_next")], 
      group = crf_train$doc_id,
      method = "lbfgs",
      file = "dietTaggerIOB.crfsuite",
      options = list(max_iterations = 250, feature.minfreq = 2, c1 = 0.1, c2 = 0.1)
    )
    
    # Return successful model training message
    invisible(c("dietTaggerIOB.crfsuite", "dietTaggerBin.crfsuite"))
  }, error = function(e) {
    # Handle errors during model training
    cat("Error in training the model:", conditionMessage(e), "\n")
    return(invisible(NULL))
  }, warning = function(w) {
    # Handle warnings during model training
    cat("Warning in training the model:", conditionMessage(w), "\n")
    return(invisible(NULL))
  })
}


# Call the training function
#train.model()

food.annotator.crf <- function(text) {
  # Check if input 'text' is valid
  stopifnot(is.data.frame(text) && ncol(text) >= 2 && is.character(text[, 2]))
  
  # Check for the existence of required model files and train them if missing
  train_models_if_missing()
  
  # Preprocess the text data
  crf_test <- preprocess(text)
  
  # Predict labels using both models
  scores_ner <- predict(modelisNER, newdata = crf_test, group = crf_test$doc_id)
  scores_iob <- predict(modelIOB, newdata = crf_test, group = crf_test$doc_id)
  
  crf_test$isNER <- scores_ner$label
  crf_test$labels <- scores_iob$label
  
  taglist <- data.frame(PMID = integer(), annotation = character(), stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(crf_test))) {
    if (crf_test$labels[i] == "B" & crf_test$isNER[i] == "1") {
      tag <- crf_test$token[i]
      
      while (crf_test$labels[i + 1] == "I" & crf_test$isNER[i + 1] == "1") {
        i <- i + 1
        tag <- paste(tag, crf_test$token[i], sep = " ")
      }
      
      tag <- clean_all(tag)
      # Remove leading/trailing spaces and convert to lowercase for consistency
      tag <- stringr::str_trim(tag)
      
      taglist <- rbind(taglist, data.frame(PMID = crf_test[i, "PMID"], annotation = tag))
    }
  }
  
  return(taglist)
}

# Function to train models if they don't exist
train_models_if_missing <- function() {
  sucess <- TRUE
  model_files <- c("dietTaggerIOB.crfsuite", "dietTaggerBin.crfsuite")
  
  missing_models <- model_files[!file.exists(model_files)]
  
  if (length(missing_models) > 0) {
    tryCatch({
      invisible(lapply(missing_models, train.model))
    }, error = function(e) {
      sucess <- FALSE
      cat("Error in training models:", conditionMessage(e), "\n")
    })
  }
  
  if (sucess) {
    for (model_file in model_files) {
      assign(tools::file_path_sans_ext(model_file), as.crf(file = model_file))
    }
  }
}

preprocess_text <- function(text) {
  # Convert to data.frame and coerce text column to character
  texts <- data.frame(text = as.character(text[, 2]), stringsAsFactors = FALSE)
  names(texts) <- as.character(text[, 1])
  
  # spacy_parse to extract text features
  output <- spacy_parse(trimws(texts$text), pos = TRUE, lemma = FALSE, tag = TRUE, 
                        entity = FALSE, dependency = TRUE, nounphrase = TRUE)
  output <- as.data.table(output)
  
  # Calculate lagged and leaded features
  output[, c("pos_previous", "token_previous", "pos_previous_prev") := shift(.SD, n = 1, type = "lag"), by = doc_id]
  output[, c("pos_next", "token_next", "pos_next_next") := shift(.SD, n = 1, type = "lead"), by = doc_id]
  
  # Create feature strings
  output[, c("pos_previous", "pos_next", "token_previous", "token_next", "pos_previous_prev", "pos_next_next") := lapply(.SD, function(x) sprintf("%s", x)), 
         .SDcols = c("pos_previous", "pos_next", "token_previous", "token_next", "pos_previous_prev", "pos_next_next")]
  
  # Flag if Part-of-Speech tag starts with "NN"
  output[, isNN := str_starts(tag, "NN")]
  
  # Set inDict flag to 0
  output[, inDict := "0"]
  
  # Dictionary lookups - use vectorized functions for improved efficiency
  dietary_words <- tolower(dietaryDictionary)
  output[, inDict := as.integer(token %in% dietary_words | stem_words(tolower(token)) %in% dietary_words)]
  
  # Clean up nounphrase column
  output[, nounphrase := factor(nounphrase, levels = c("O", "B", "I", "beg", "mid", "end_root", "beg_root"))]
  output[nounphrase == "beg" & pos == "DET", nounphrase := "O"]
  output[, nounphrase := forcats::fct_collapse(nounphrase, O = "O", B_I = "I")]
  
  return(output)
}

process_labels <- function(fulltexts, annotation, output1) {
  # Merge annotation with fulltexts on doc_id
  annotation_df <- data.frame(doc_id = fulltexts[, 1], annotation = annotation[, 2])
  output1 <- merge(output1, annotation_df, by = "doc_id", all.x = TRUE)
  
  # Function to handle NER tagging for a given document
  tag_NER_doc <- function(doc_id, annota_all, output_sub) {
    pos_loc <- output_sub[order(loc), .(loc, pos)]
    pos_loc <- rbindlist(list(pos_loc, data.table(loc = 145, pos = "-")), use.names = TRUE)
    
    for (j in seq_len(nrow(pos_loc))) {
      pos <- pos_loc$pos[j]
      
      if (!grepl("\\d", pos)) {
        tag1 <- grep(paste0("\\b", pos, "\\b"), annota_one)
        if (length(tag1) > 0) {
          output_sub$isNER[j] <- 1
        }
        
        tag <- grep(paste0("\\b", pos, "\\b"), annota_all)
        if (length(tag) > 0) {
          for (t in seq_along(tag)) {
            annota_toks <- unlist(str_split(annota_all[tag[t]], boundary("word")))
            annota_toks <- annota_toks[nchar(annota_toks) > 0]
            wrd_count <- length(annota_toks)
  
            if (grepl("-", annota_all[tag[t]])) {
              wrd_count <- wrd_count + sum(grepl("-", annota_toks))
            }
  
            comp_wrd <- trimws(paste(pos_loc$pos[j:(j + wrd_count - 1)], collapse = " "))
            comp_wrd <- gsub(" - ", "-", comp_wrd, fixed = TRUE)
  
            if (comp_wrd == annota_all[tag[t]]) {
              output_sub$isNER[j:(j + wrd_count - 1)] <- 1
              j <- j + wrd_count - 1
            }
          }
        }
      }
    }
    
    return(output_sub)
  }
  
  # Apply tag_NER_doc function by doc_id
  output1 <- output1[, tag_NER_doc(doc_id, annota_all = strsplit(annotation, ","), .SD), by = doc_id]
  
  return(output1)
}
