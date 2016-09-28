#' UClassifier Internals
#'
#' Internal functions for the Reference Class UClassifier.
#' (DO NOT EXPORT)
#'
#' @import httr
#' @import jsonlite
#'
## Helper Functions -------------------------------------------------------------
.base_url <- function() {
    return("https://api.uclassify.com/v1/")
}

.response_to_df <- function(response, ...) {
    df <- do.call("rbind", lapply(content(response), data.frame))
    return(df)
}

.GET_request <- function(url, content, token, ...) {
    return(GET(url, body=content,
               add_headers(Authorization=paste("Token", token)),
               content_type_json(), ...))
}

.POST_request <- function(url, content, token, ...) {
    return(POST(url, body=content,
                add_headers(Authorization=paste("Token", token)),
                content_type_json(), ...))
}

.DELETE_request <- function(url, token, ...) {
    return(DELETE(url,
                  add_headers(Authorization=paste("Token", token)),
                  content_type_json(), ...))
}

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

.API.classifier_exists <- function(object) {
    response <- .API.get_information(object)
    return(.is_OK_response(response))
}

.is_OK_response <- function(response) {
    status_code <- response$status_code
    return(status_code >= 200 && status_code < 300)
}

## Initialization ---------------------------------------------------------------
.initialize <- function(object, classifier_name, username, read_token, write_token) {
    object$classifier_name <- classifier_name
    object$username <- username
    object$read_token <- read_token
    object$write_token <- write_token
    object$cache$dirty <- TRUE

    .create_classifier(object)
    .cache.initialize(object)
}

.cache.initialize <- function(object, ...) {
    information <- object$get_information()

    for (class in information$className) {
        .cache.add_class(object, class)
    }

    return(invisible())
}

## Read Methods -----------------------------------------------------------------
.get_information <- function(object, ...) {
    if (object$cache$dirty) {
        response <- .API.get_information(object)
        object$cache$information <- .response_to_df(response)
        object$cache$dirty <- FALSE
    }

    return(cache$information)
}

.API.get_information <- function(object, ...) {
    "base_url/username/classifier_name"
    url <- paste(.base_url(), paste(object$username, object$classifier_name, sep="/"), sep="")
    return(.GET_request(url, NULL, object$read_token))
}

## Write Methods ----------------------------------------------------------------
.create_classifier <- function(object, ...) {
    if (!(.API.classifier_exists(object))) {
        url <- paste(.base_url(), "me/", sep="")
        content <- toJSON(list(classifierName=object$classifier_name))
        response <- .POST_request(url, content, object$write_token)

        if (!(.is_OK_response(response))) {
            stop(content(response)$message)
        }
    }

    return(invisible())
}

.remove_classifier <- function(object, ...) {
    if (.API.classifier_exists(object)) {
        url <- paste(.base_url(), "me/", object$classifier_name, sep="")
        response <- .DELETE_request(url, object$write_token)

        if (!(.is_OK_response(response))) {
            stop(content(response)$message)
        }
    }

    return(invisible())
}

.add_class <- function(object, class, ...) {
    formatted_class <- .format.class_input(class)

    for (class in formatted_class) {
        if (!(.class_exists(object, class))) {
            .cache.add_class(object, class)
            response <- .API.add_class(object, class)
        }
    }

    return(invisible())
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

.API.add_class <- function(object, class, ...) {
    url <- paste(.base_url(), "me/", object$classifier_name, "/addClass", sep="")
    content <- toJSON(list(className=class))
    response <- .POST_request(url, content, object$write_token)

    if (!(.is_OK_response(response))) {
        warning(content(response)$message)
    } else {
        ## This is a lazy approach
        ## It would be better to add the class directly to the cache$information as well
        object$cache$dirty <- TRUE
    }

    return(invisible())
}


.remove_class <- function(object, class, ...) {
    formatted_class <- .format.class_input(class)

    for (class in formatted_class) {
        if (.class_exists(object, class)) {
            .cache.remove_class(object, class)
            .API.remove_class(object, class)
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

.API.remove_class <- function(object, class, ...) {
    url <- paste(.base_url(), "me/", paste(object$classifier_name, class, sep="/"), sep="")
    reponse <- .POST_request(url, NULL, object$write_token)

    if (!(.is_OK_response(response))) {
        warning(content(response)$message)
    } else {
        ## This is a lazy approach
        ## It would be better to remove the class directly to the cache$information as well
        object$cache$dirty <- TRUE
    }

    return(invisible())
}


.train <- function(object, text, class, ...) {

}

.untrain <- function(object, text, class, ...) {

}
