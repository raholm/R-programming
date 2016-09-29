library(tm)
library(wordcloud)
library(memoise)
library(Lab5)

valid_classes <<- c("negative", "neutral", "positive")

## Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(class) {
    if (!(class %in% valid_classes)) {
        stop("Unknown class")        
    }
    polarity_value <- (which(valid_classes == class) - 1) * 2
    text <- as.character(Lab5::test_tweets$text[which(test_tweets$polarity == polarity_value)])
    
    corpus <- Corpus(VectorSource(text))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))

    dtm <- TermDocumentMatrix(corpus,
                              control = list(minWordLength = 1))
    dtm <- as.matrix(dtm)
    
    sort(rowSums(dtm), decreasing = TRUE)
})
