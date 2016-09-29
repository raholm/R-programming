#' UClassifier
#'
#' UClassifier is a text classifier that uses the rest-API provided by uclassify.com
#'
#' @field classifier_name The name of the classifier.
#' @field username Your username on the website.
#' @field read_token Your read token to access the API.
#' @field write_token  Your write token to access the API.
#'
#' @import methods
#'
#' @export UClassifier
#' @exportClass UClassifier
#'
#' @source \url{https://uclassify.com/docs}
UClassifier <- setRefClass("UClassifier",
                           fields=list(
                               classifier_name="character",
                               username="character",
                               read_token="character",
                               write_token="character",
                               cache="list"
                           ))

## Constructor
UClassifier$methods(list(
                initialize = function(classifier_name, username, read_token, write_token, ...) {
                    .initialize(.self, classifier_name, username, read_token, write_token)
                    callSuper(...)
                }
            ))

## Read Calls
UClassifier$methods(list(
                get_information = function() {
                    return(.get_information(.self))
                },
                classify = function(text) {
                    return(.classify(.self, text))
                },
                get_keywords = function(text) {
                    return(.get_keywords(.self, text))
                }
            ))

## Write Calls
UClassifier$methods(list(
                add_class = function(class) {
                    .add_class(.self, class)
                    return(invisible())
                },
                remove_class = function(class) {
                    .remove_class(.self, class)
                    return(invisible())
                },
                train = function(text, class) {
                    .train(.self, text, class)
                    return(invisible())
                },
                untrain = function(text, class) {
                    .untrain(.self, text, class)
                    return(invisible())
                }
            ))


#' Delete Classifier
#'
#' Function for delete your classifier
#'
#' @field classifier The UClassifier to delete.
#'
#' @export
remove_classifier <- function(classifier) {
  .remove_classifier(classifier)
}
