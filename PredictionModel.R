library(dplyr)


prediction <- function(inputText, Ngrams) {
  
  
  s<-1:3
  lambdas <- c(0.05, 0.20, 0.30, 0.45)
  tokens<-unlist(strsplit(inputText," "))
  tokens <- sapply(s, function(i) tail(tokens,i))
  
  for (i in length(Ngrams):1) {
    tmp <- Ngrams[[i]]
    tok <- paste(tokens[[i-1]],collapse = "_")
    tok <- paste(tok, "_", sep = "")
    #which(tmp$token == tok)
    tmp <- tmp[grep(tok,tmp$token),]
    #tmp <- mutate(tmp, prob = rcount/N)
    #tmp <- arrange(tmp, desc(count))
    tmp <- tmp %>% mutate(token = gsub(tok, "", token), prob = (rcount/N)*lambdas[i]) %>%
      arrange(desc(prob)) %>% select(token, prob)
    #tmp <- head(tmp)
    predict <- tmp
  }
  
  head(predict)
}


load("Corpus_NGrams_Frequencies_R.RData")
prediction("Do you want to go to the", NGrams)