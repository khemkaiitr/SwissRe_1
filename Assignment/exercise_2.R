rm(list = ls())
library(SnowballC)
library(tm)
library(plyr)
library(class)
library(e1071)

# Check the current directory, download the data if does not exist and unzip it
dataDir <-  getwd()
if (!file.exists(file.path(dataDir, 'bbc'))){
  dataUrl <- 'http://mlg.ucd.ie/files/datasets/bbc-fulltext.zip'
  download.file(dataUrl, destfile = file.path(dataDir, 'bbc.zip'))
  unzip('bbc.zip', exdir = dataDir)
}

# Folder names/ or class names
cat = c("business", "sport", "entertainment", "politics", "tech")

# Function to load and preprocess data
load_preprocess <- function(cate, path){
  s.path <- file.path(path, "bbc", cate)
  s.cor <- Corpus(DirSource(s.path))
  ncorpus <- tm_map(s.cor, removePunctuation)
  for(j in seq(ncorpus))   
  {   
    ncorpus[[j]] <- gsub("/", " ", ncorpus[[j]])   
    ncorpus[[j]] <- gsub("@", " ", ncorpus[[j]])   
    ncorpus[[j]] <- gsub("\\|", " ", ncorpus[[j]])   
  }  
  # Remove punctuations, convert to lower case and remove stop words
  docs <- tm_map(ncorpus, removeNumbers)   
  docs <- tm_map(docs, tolower)   
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stemDocument)   
  docs <- tm_map(docs, stripWhitespace)   
  docs <- tm_map(docs, PlainTextDocument) 
  dtm <- DocumentTermMatrix(docs)   
  tdm <- TermDocumentMatrix(docs)   
  tdm.common = removeSparseTerms(tdm, 0.8)
  result <-list(name= cate,tdm = tdm.common)
}

# Function to bind all the categories, with the name of the classes
bindCategoryTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat,stringsAsFactors = F)
  s.df <- cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetCat"
  return(s.df)
}

# Read, load and preprocess all the text data from different folder
corpus <- lapply(cat, load_preprocess, path = dataDir)
catTDM <- suppressWarnings(lapply(corpus,bindCategoryTDM)) # Read processed corpus and create a data frame

#Stack all the concatanted TDMs and replace NAs with zero
tdm.stack <- do.call(rbind.fill,catTDM)
tdm.stack[is.na(tdm.stack)]  <- 0

## Do the actual stuff 
# I am dividing data for 70% train and 30 % test set
set.seed(26062016)
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack))[-train.idx]

tdm.cat<- tdm.stack[,"targetCat"]
tdm.stack.nl<- tdm.stack[,!colnames(tdm.stack) %in% "targetCat"]

# Using svm to create the model and test the accuracy on the test dataset
# Please note that current model is restricted to default svm parameters from R. So the Kernel is RBF (radial basis function with 3 degrees). 
svm.model<- svm(x = tdm.stack.nl[train.idx,], y = tdm.cat[train.idx])
svm.pred <- predict(svm.model, newdata = tdm.stack.nl[test.idx, ])
conf.mat<- table("predictions" = svm.pred,Actual = tdm.cat[test.idx])
accuracy<- sum(diag(conf.mat)/length(test.idx) *100)

cat("Accuracy of the classification is: ", accuracy, "\n")
conf.mat


