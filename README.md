**README: Food Annotator with CRF**

This project provides a Named Entity Recognition (NER) and Inside-Outside-Beginning (IOB) tagging solution for dietary information extraction from text. It uses Conditional Random Fields (CRF) models trained on annotated data to identify and classify food-related entities.

**Key Features**

* **Named Entity Recognition (NER):** Identifies and classifies food-related entities in text.
* **Inside-Outside-Beginning (IOB) Tagging:** Assigns labels to words within food entities (e.g., "B-food" for the beginning of a food entity, "I-food" for inside).
* **Spacy Integration:** Utilizes the Spacy library for natural language processing and feature extraction.
* **Customizable Dictionary:** Allows for expanding the recognized food terms through a user-defined dictionary.

**Requirements**

* **R (version 3.6 or later)**
* **R Packages:**
    * `crfsuite`
    * `stringr`
    * `spacyr` (with Spacy model installed)
    * `data.table` (for efficient data manipulation)
    * `utils` (for loading data)


**Installation**

1. **Install R Packages:**
   ```R
   install.packages(c("crfsuite", "stringr", "spacyr", "data.table", "utils"))
   ```
2. **Install Spacy and Load Model:**
   ```R
   library(spacyr)
   spacy_install()  # Installs Spacy if not already present
   spacy_download_langmodel("en_core_web_sm")  # Downloads the English language model
   ```

**Data**

* **`dietaryDictionary.Rdata`:** Contains a dictionary of food-related terms.
* **`foodbase_features_for_CRF.Rdata`:** Includes preprocessed data with features for training and testing the model.


**Usage**

1. **Load Data and Train Model:**
   ```R
   train.model()  # Trains the CRF model
   ```
2. **Annotate Text:**
   ```R
   text_to_annotate <- "I ate a delicious apple pie for dessert."
   annotations <- food.annotator.crf(data.frame("PMID" = 1, "text" = text_to_annotate))
   print(annotations)
   ```
   

**Output**

* **`taglist`:** A dataframe with the identified food-related entities and their corresponding types.

**Important Notes**

* Ensure that the required data files (`dietaryDictionary.Rdata` and `foodbase_features_for_CRF.Rdata`) are in your working directory.
* The `preprocess` and `process_labels` functions are used internally for data preparation and labeling.
* If the model files (`dietTaggerIOB.crfsuite` and `dietTaggerBin.crfsuite`) don't exist, the `food.annotator.crf` function will automatically train the models before proceeding with annotation.
