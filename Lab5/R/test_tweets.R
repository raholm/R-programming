#' Twitter Tweets
#'
#' A collection of twitter tweets gathered and classified by Alec Go, Richa Bhayani, and Lei Huang, see the source.
#'
#' The collection contains around 500 tweets of test data that have been classified either as negative, neutral, or positive. There is a training data set if you follow the link containing around 160 000 tweets. 
#'
#' @format A data frame containing 6 fields
#' \describe{
#' \item{polarity}{The polarity of the tweet (0 = negative, 2 = neutral, 4 = positive).}
#' \item{id}{The id of the tweet.}
#' \item{date}{The date of the tweet.}
#' \item{query}{The query.}
#' \item{user}{The user of the tweet.}
#' \item{text}{The text content of the tweet.}
#' }
#'
#' @source \url{http://help.sentiment140.com/for-students}
"test_tweets"

