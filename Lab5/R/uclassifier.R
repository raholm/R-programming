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

                },
                classify=function(text) {
                    "Text input should look like this: list(texts=c(\"text 1 to process", "text 2 to process\"))"
                },
                get_keywords=function(text) {
                    "Text input should look like this: list(texts=c(\"text 1 to process", "text 2 to process\"))"
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
                    "Text input should look like this: list(texts=c(\"text 1 to process", "text 2 to process\"))"
                },
                untrain=function(text, class) {
                    "Check that class exists using the cache in order to reduce the number of API calls."
                    "Text input should look like this: list(texts=c(\"text 1 to process", "text 2 to process\"))"
                }
            ))
