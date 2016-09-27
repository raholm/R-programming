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
'VALID'
c('VALID1', 'VALID2')
list(text='VALID')
list(text=c('VALID1', 'VALID2'))
data.frame(text='VALID')
data.frame(text=c('VALID1', 'VALID2'))
"
}

.format.text_input <- function(text) {
}

.to_json.text_input <- function(text) {
"
Input : 'VALID'
Output : {'texts':['VALID']}
Input : c('VALID1', 'VALID2')
Output : {'texts':['VALID1', 'VALID2']}
The same for list and data.frame

(USE jsonlite library)
"
}

.check.class_input <- function(class) {
"
Valid inputs should look like this:
'VALID'
c('VALID1', 'VALID2')
list(class='VALID')
list(class=c('VALID1', 'VALID2'))
data.frame(class='VALID')
data.frame(class=c('VALID1', 'VALID2'))
"
}
