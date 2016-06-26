
rm(list = ls())
library(SnowballC)
library(tm)
library(plyr)
library(class)
library(e1071)
dataDir <-  getwd()
if (!file.exists(file.path(dataDir, 'bbc'))){
  dataUrl <- 'http://mlg.ucd.ie/files/datasets/bbc-fulltext.zip'
  download.file(dataUrl, destfile = file.path(dataDir, 'bbc.zip'))
  unzip('bbc.zip', exdir = dataDir)
}
corpus <- Corpus(DirSource(file.path(dataDir, "bbc", "business")))
ncorpus = tm_map(corpus, removePunctuation)
for(j in seq(ncorpus))   
{   
  ncorpus[[j]] <- gsub("/", " ", ncorpus[[j]])   
  ncorpus[[j]] <- gsub("@", " ", ncorpus[[j]])   
  ncorpus[[j]] <- gsub("\\|", " ", ncorpus[[j]])   
}  
docs <- tm_map(ncorpus, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("department", "email"))   
docs <- tm_map(docs, stemDocument)   
docs <- tm_map(docs, stripWhitespace)   
docs <- tm_map(docs, PlainTextDocument)   
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   
tdm.common = removeSparseTerms(tdm, 0.8)

s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))


cleanCorpus <-function(corpus) {
  corpus.tmp = tm_map(corpus,removePunctuation)
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp = tm_map(corpus.tmp,tolower)
  corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
  corpus.tmp = tm_map(corpus.tmp,stemDocument)
  return(corpus.tmp)
}
generateTDM <- function(cate,path) {
  s.path =  file.path(path, "bbc", cate)
  s.cor =  Corpus(DirSource(s.path))
  s.cor.cl = cleanCorpus(s.cor)
  s.tdm= TermDocumentMatrix(s.cor.cl)
  s.tdm = removeSparseTerms(s.tdm,0.7)
  result <-list(name= cate,tdm = s.tdm)
}
tdm = lapply(cat,generateTDM,path = dataDir)

temp <- function(cate, path){
  s.path = file.path(path, "bbc", cate)
  print(s.path)
  s.cor =  Corpus(DirSource(s.path))
  ncorpus = tm_map(s.cor, removePunctuation)
  for(j in seq(ncorpus))   
  {   
    ncorpus[[j]] <- gsub("/", " ", ncorpus[[j]])   
    ncorpus[[j]] <- gsub("@", " ", ncorpus[[j]])   
    ncorpus[[j]] <- gsub("\\|", " ", ncorpus[[j]])   
  }  
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
corpus <- lapply(cat, temp, path = dataDir)
bindCategoryTDM <- function(tdm) {
  s.mat = t(data.matrix(tdm[["tdm"]]))
  s.df = as.data.frame(s.mat,stringsAsFactors = F)
  s.df = cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetCat"
  return(s.df)
  
}

catTDM = lapply(corpus,bindCategoryTDM)

#Stack 
tdm.stack = do.call(rbind.fill,catTDM)
tdm.stack[is.na(tdm.stack)] = 0

#holdout
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.7))
text.idx = (1:nrow(tdm.stack))[-train.idx]


#model
tdm.cat = tdm.stack[,"targetCat"]
tdm.stack.nl = tdm.stack[,!colnames(tdm.stack) %in% "targetCat"]
knn.pred = knn(tdm.stack.nl[train.idx,],tdm.stack.nl[text.idx,],tdm.cat[train.idx])
#accuracy
conf.mat = table("predictions" = knn.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat)/length(text.idx) *100)
accuracy
conf.mat 


svm.model = svm(x = tdm.stack.nl[train.idx,], y = tdm.cat[train.idx])
svm.pred = predict(svm.model, tdm.stack.nl[test.idx])
conf.mat = table("predictions" = svm.pred,Actual = tdm.cat[text.idx])
accuracy = sum(diag(conf.mat)/length(text.idx) *100)
accuracy
conf.mat

# ncorpus = tm_map(ncorpus, stripWhitespace)
# ncorpus = tm_map(ncorpus, tolower)
# 
# # I was getting multiple core error so I used Laze = TRUE
# ncorpus = tm_map(ncorpus, removeWords, stopwords, lazy = TRUE)
# tdm = TermDocumentMatrix(ncorpus, lazy = TRUE)
