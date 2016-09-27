#' UClassifier Internals
#'
#' Internal functions for the Reference Class UClassifier.
#' (DO NOT EXPORT)
#'
#' @import httr
#' @import jsonlite
## Helper Functions -------------------------------------------------------------
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
.check.text_input(text)
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
formatted_text <- .format.text_input(text)
return(formatted_text)
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

.format.class_input <- function(class) {
"
Input : 'CLASS'
Output : 'CLASS'
Input : c('CLASS1', 'CLASS2')
Output : c('CLASS1', 'CLASS2')
Input : list(class='CLASS')
Output : 'CLASS'
Input : list(class=c('CLASS1', 'CLASS2')
Output : c('CLASS1', 'CLASS2')
Same with other valid inputs.
"
.check.class_input(class)
return(class)
}

.cache.add_class <- function(object, class) {
    if (!("class" %in% names(object$cache))) {
        object$cache$class <- class
    } else {
        if (!(class %in% object$cache$class)) {
            object$cache$class <- c(object$cache$class, class)
        }
    }
    return(invisible())
}

.cache.remove_class <- function(object, class) {
    if (!("class" %in% names(object$cache))) {
        return(invisible())
    }

    if (length(object$cache$class) == 1 && object$cache$class == class) {
        object$cache$class <- NULL
    } else {
        object$cache$class <- object$cache$class[-which(object$cache$class == class)]
    }

    return(invisible())
}

## Read Methods -----------------------------------------------------------------
.get_information <- function(object, ...) {

}

## Write Methods ----------------------------------------------------------------
.create <- function(object, ...) {

}

.remove <- function(object, ...) {

}

.add_class <- function(object, class, ...) {
    formatted_class <- .format.class_input(class)

    for (class in formatted_class) {
        if (!(.class_exists(object, class))) {
            .cache.add_class(object, class)
        }
    }

    return(invisible())
}

.remove_class <- function(object, class, ...) {
    formatted_class <- .format.class_input(class)

    for (class in formatted_class) {
        if (.class_exists(object, class)) {
            .cache.remove_class(object, class)
        }
    }

    return(invisible())
}

.train <- function(object, text, class, ...) {

}

.untrain <- function(object, text, class, ...) {

}
