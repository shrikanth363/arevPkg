analyseReview <- function(review = ""){
  Text <- c("dummy",review)
  #names(e)  <- "Text"
  id <- c(1,2)
  #e <- data.frame(id,Text)
  e <- data.frame(id,Text)
  e_corpus <- VCorpus(VectorSource(e$Text))
  #as.character(e_corpus[[1]])
  #lapply(e_corpus[1:2],as.character)
  e_dtm <- DocumentTermMatrix(e_corpus, control = list(tolower = TRUE,removeNumbers = TRUE,stopwords = TRUE,removePunctuation = TRUE,stemming = TRUE))
  #str(e_dtm)
  e_test  <- apply(e_dtm, MARGIN = 2, convert_counts)
  
  e_test_pred <- predict(review_classifier, e_test)
  e_test_pred[2]
  
  if(e_test_pred[2] == ""){
    stop("Error")
  }
  
  list(
    message = paste(e_test_pred[2]))
  
}


# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
# end of testing

