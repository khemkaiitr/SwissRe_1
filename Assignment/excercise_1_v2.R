rm(list = ls())
library(pdftools)
library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

# File name of the pdf that will be used for mining
filename <- "2015_financial_report_swissre_ar15.pdf"

# Read the pdf file 
my_pdf_txt <- pdf_text("2015_financial_report_swissre_ar15.pdf")

#
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
location_ann <- Maxent_Entity_Annotator(kind = "location", probs = TRUE)
pipeline <- list(sent_ann,
                 word_ann,
                 location_ann)

# Initialise result data frame where data will be stored
resultFrame <- NULL

# Starting from 6th page given
for (i in 1:length(my_pdf_txt)){
  bio <- as.String(my_pdf_txt[i])
  
  if (nchar(bio) >1){ # Check for the length of a given page
    bio_annotations <- annotate(bio, pipeline)
    
    k <- sapply(bio_annotations$features, '[[', "kind")
    
    if (any(k == "location")){
      loc_names = unique(bio[bio_annotations[k == "location"]])
      # Some preprocessing of the data
      
      loc_names = gsub("[[:digit:]]", "", loc_names)
      loc_names = gsub("[[:punct:]]", "", loc_names) # Additional preprocessing to remove punctuation characters
      loc_names <- gsub("[\r\n\t]", "", loc_names)
      
      # Save data in temporary variable temp and store them in the final dataframe
      temp = data.frame(pageno = i, location = loc_names,
                        stringsAsFactors=FALSE)
      resultFrame <- rbind(resultFrame, temp)
      # Remove variables
      rm(temp, loc_names)
    }
    rm(k, bio, bio_annotations) # remove variables
  }
}

# Remove row with no characters in the location field
resultFrame <- resultFrame[- which(resultFrame$location == ''), ]

# Print the result
for (i in unique(resultFrame$location)){
  cat(i, "-> Found on page no",
      unique(resultFrame$pageno[which(resultFrame$location == i)]), '\n')
}
