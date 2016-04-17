library(dplyr)


prediction <- function(inputText, Ngrams) {
  
  
  s<-1:3
  lambdas <- c(0.45, 0.30, 0.20, 0.05)
  words<-unlist(strsplit(inputText," "))
  tokens <- sapply(s, function(i) tail(words,i))
  
  
  
  g<-tail(g,l)
  g<-paste(g,collapse = "_")
  
}