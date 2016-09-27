#' UClassifier Internals
#'
#' Internal functions for the Reference Class UClassifier.
#' (DO NOT EXPORT)
#'
#' @import httr
#' @import jsonlite

.check.text_input <- function(text) {
"
Valid inputs should look like this:
'TEXT'
c('TEXT1', 'TEXT2')
list(text='TEXT')
list(text=c('TEXT1', 'TEXT2'))
data.frame(text='TEXT')
data.frame(text=c('TEXT1', 'TEXT2'))
"
return(invisible())
}

.format.text_input <- function(text) {
"
Input : 'TEXT'
Output : list(texts='TEXT')
Input : c('TEXT1', 'TEXT2')
Output : list(texts=c('TEXT1', 'TEXT2'))
The same for the other valid text inputs
"
return(text)
}

.to_json.text_input <- function(text) {
"
Input : 'TEXT'
Output : {'texts':['TEXT']}
Input : c('TEXT1', 'TEXT2')
Output : {'texts':['TEXT1', 'TEXT2']}
The same for the other valid text inputs

(Use jsonlite library)
"
return(text)
}

.check.class_input <- function(class) {
"
Valid inputs should look like this:
'CLASS'
c('CLASS1', 'CLASS2')
list(class='CLASS')
list(class=c('CLASS1', 'CLASS2'))
data.frame(class='CLASS')
data.frame(class=c('CLASS1', 'CLASS2'))
"
return(invisible())
}
