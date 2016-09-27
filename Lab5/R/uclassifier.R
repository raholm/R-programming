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
                initialize=function(classifier_name, username, read_token, write_token, ...) {
                    callSuper(...)
                }
            ))

## Read Calls
UClassifier$methods(list(
                get_information=function() {
                    return(.get_information(.self))
                },
                classify=function(text) {
                },
                get_keywords=function(text) {
                }
            ))

## Write Calls
UClassifier$methods(list(
                add_class=function(class) {
                    "Cache the class"
                },
                remove_class=function(class) {
                    "Don't forget to remove the class from the cache"
                },
                train=function(text, class) {
                    "Check that class exists using the cache in order to reduce the number of API calls."
                },
                untrain=function(text, class) {
                    "Check that class exists using the cache in order to reduce the number of API calls."
                }
            ))
