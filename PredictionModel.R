library(quanteda)
library(dplyr)


prediction <- function(inputText, Ngrams) {
  
  predicts <- data.frame(token = character(), prob = double(), stringsAsFactors = FALSE)
  
  tokens <- tokenize(toLower(inputText),
                     removePunct = TRUE,
                     removeSeparators = TRUE,
                     removeNumbers = TRUE,
                     verbose = FALSE,
                     simplify = TRUE)
  
  if (length(tokens) < 3) s <- 1:length(tokens) else s<-1:3
  
  tokens <- sapply(s, function(i) tail(tokens,i))
  
  lambdas <- c(0.05, 0.15, 0.30, 0.50)
  
  
  for (i in length(Ngrams):2) {
    tmp <- Ngrams[[i]]
    tok <- paste(tokens[[i-1]],collapse = "_")
    tok <- paste(tok, "_", sep = "")
    
    tmp <- tmp[grep(tok,tmp$token),]
    tmp <- tmp %>% mutate(token = gsub(tok, "", token), prob = (rcount/N)*lambdas[i]) %>%
      arrange(desc(prob)) %>% select(token, prob)
    
    predicts <- full_join(tmp, predicts)
  }
  
  predicts <- predicts %>% group_by(token) %>% 
    summarize(prob = sum(prob)) %>% arrange(desc(prob))
  #head(predicts)
}


load("Corpus_NGrams_Frequencies_R.RData")
a<-prediction("Do you want to go to the", NGrams)