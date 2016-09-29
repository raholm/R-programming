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

.response_content_to_df <- function(response, ...) {
    df <- do.call("rbind", lapply(content(response), data.frame, stringsAsFactors=FALSE))
    return(df)
}

.add_response_message <- function(object, response, ...) {
    if (!(.is_OK_response(response))) {
        object$cache$APImessage <- content(response)$message

        ## Seems to be some inconsistency with the API
        if (is.null(object$cache$APImessage)) {
            object$cache$APImessage <- content(response)$Message
        }
    }
}

.class_exists <- function(object, class, ...) {
    if (is.null(object$cache$information)){
        return(FALSE)
    }

    return(class %in% object$cache$information$className)
}

.warn <- function(msg) {
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    warning(msg)
    return(invisible())
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

"
Valid inputs should look like this:
'TEXT'
c('TEXT1', 'TEXT2')
list(text='TEXT')
list(text=c('TEXT1', 'TEXT2'))
data.frame(text='TEXT')
data.frame(text=c('TEXT1', 'TEXT2'))
"
.check.text_input <- function(text) {
    valid <- FALSE

    if ("text" %in% names(text) && is.character(text$text)){
        valid <- TRUE
    } else if(is.character(text)) {
        valid <- TRUE
    }

    if (!valid) {
        stop("Invalid text input.")
    }

    return(invisible())
}

"
Input : 'TEXT'
Output : list(texts='TEXT')
Input : c('TEXT1', 'TEXT2')
Output : list(texts=c('TEXT1', 'TEXT2'))
The same for the other valid text inputs
"
.format.text_input <- function(text) {
    .check.text_input(text)

    if ("text" %in% names(text)) {
        text <- text$text
    }

    return(list(texts=text))
}

"
Input : 'TEXT'
Output : {'texts':['TEXT']}
Input : c('TEXT1', 'TEXT2')
Output : {'texts':['TEXT1', 'TEXT2']}
The same for the other valid text inputs

(Use jsonlite library)
"
.to_json.text_input <- function(text) {
    formatted_text <- .format.text_input(text)
    return(toJSON(formatted_text))
}


"
Valid inputs should look like this:
'CLASS'
c('CLASS1', 'CLASS2')
list(class='CLASS')
list(class=c('CLASS1', 'CLASS2'))
data.frame(class='CLASS')
data.frame(class=c('CLASS1', 'CLASS2'))
"
.check.class_input <- function(class) {
    valid <- FALSE

    if ("class" %in% names(class) && is.character(class$class)){
        valid <- TRUE
    } else if(is.character(class)) {
        valid <- TRUE
    }

    if (!valid) {
        stop("Invalid class input.")
    }

    return(invisible())
}

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
.format.class_input <- function(class) {

    .check.class_input(class)

    if ("class" %in% names(class)) {
        class <- class$class
    }

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

    object$cache <- list()
    object$cache$dirty <- TRUE

    .create_classifier(object)
    .cache.initialize(object)
}

.cache.initialize <- function(object, ...) {
    .get_information(object)
    return(invisible())
}

## Read Methods -----------------------------------------------------------------
.get_information <- function(object, ...) {
    if (object$cache$dirty) {
        response <- .API.get_information(object)

        if (!(.is_OK_response(response))) {
            msg <- object$cache$APImessage
            .warn(msg)
            return(NULL)
        }

        object$cache$information <- .response_content_to_df(response)
        object$cache$dirty <- FALSE
    }

    return(object$cache$information)
}

.API.get_information <- function(object, ...) {
    "base_url/username/classifier_name"
    url <- paste(.base_url(), paste(object$username, object$classifier_name, sep="/"), sep="")
    response <- .GET_request(url, NULL, object$read_token)

    .add_response_message(object, response)
    return(response)
}

.classify <- function(object, text, ...) {
    response <- .API.classify(object, text)
    result <- NULL

    if (!(.is_OK_response(response))) {
        msg <- object$cache$APImessage
        .warn(msg)
    } else {
        result <- .response_content_to_df(response)
    }

    return(result)
}

.API.classify <- function(object, text, ...) {
    url <- paste(.base_url(), paste(object$username, object$classifier_name, sep="/"), "/classify", sep="")
    content <- .to_json.text_input(text)
    response <- .POST_request(url, content, object$read_token)

    .add_response_message(object, response)
    return(response)
}

.get_keywords <- function(object, text, ...) {
    response <- .API.get_keywords(object, text)
    result <- NULL

    if (!(.is_OK_response(response))) {
        msg <- object$cache$APImessage
        .warn(msg)
    } else {
        result <- .response_content_to_df(response)
    }

    return(result)
}

.API.get_keywords <- function(object, text, ...) {
    url <- paste(.base_url(), paste(object$username, object$classifier_name, sep="/"), "/keywords", sep="")
    content <- .to_json.text_input(text)
    response <- .POST_request(url, content, object$read_token)

    .add_response_message(object, response)
    return(response)
}

## Write Methods ----------------------------------------------------------------
.create_classifier <- function(object, ...) {
    if (!(.API.classifier_exists(object))) {
        url <- paste(.base_url(), "me/", sep="")
        content <- toJSON(list(classifierName=object$classifier_name), auto_unbox=TRUE)
        response <- .POST_request(url, content, object$write_token)

        if (!(.is_OK_response(response))) {
            stop(content(response)$message)
        }
    }

    return(invisible())
}

.remove_classifier <- function(object, ...) {
    stopifnot(class(object) == "UClassifier")

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
            if (!(.API.add_class(object, class))) {
                msg <- object$cache$APImessage
                .warn(msg)
            } else {
                .cache.add_class(object, class)
            }
        }
    }

    return(invisible())
}

.cache.add_class <- function(object, class) {
    if (!("information" %in% names(object$cache))) {
        object$cache$information <- data.frame(className=class, uniqueFeatures=0, totalCount=0,
                                               stringsAsFactors=FALSE)
    } else {
        if (!(class %in% object$cache$information$className)) {
            class <- data.frame(className=class, uniqueFeatures=0, totalCount=0,
                                stringsAsFactors=FALSE)
            object$cache$information <- rbind(object$cache$information, class)
        }
    }

    return(invisible())
}

.API.add_class <- function(object, class, ...) {
    url <- paste(.base_url(), "me/", object$classifier_name, "/addClass", sep="")
    content <- toJSON(list(className=class), auto_unbox=TRUE)
    response <- .POST_request(url, content, object$write_token)

    .add_response_message(object, response)
    return(.is_OK_response(response))
}


.remove_class <- function(object, class, ...) {
    formatted_class <- .format.class_input(class)

    for (class in formatted_class) {
        if (.class_exists(object, class)) {
            if (!(.API.remove_class(object, class))) {
                msg <- object$cache$APImessage
                .warn(msg)
            } else {
                .cache.remove_class(object, class)
            }
        }
    }

    return(invisible())
}

.cache.remove_class <- function(object, class) {
    if (!("information" %in% names(object$cache))) {
        return(invisible())
    }

    if (nrow(object$cache$information) == 1 && object$cache$information$className == class) {
        object$cache$information <- NULL
    } else {
        object$cache$information <- object$cache$information[-which(object$cache$information$className == class), ]
    }

    return(invisible())
}

.API.remove_class <- function(object, class, ...) {
    url <- paste(.base_url(), "me/", paste(object$classifier_name, class, sep="/"), sep="")
    response <- .DELETE_request(url, object$write_token)

    .add_response_message(object, response)
    return(.is_OK_response(response))
}


.train <- function(object, text, class, ...) {
    .train_untrain_template(object, text, class, .API.train)
}

.API.train <- function(object, text, class, ...) {
    url <- paste(.base_url(), "me/", paste(object$classifier_name, class, sep="/"), "/train", sep="")
    content <- .to_json.text_input(text)
    response <- .POST_request(url, content, object$write_token)

    .add_response_message(object, response)
    return(.is_OK_response(response))
}

.untrain <- function(object, text, class, ...) {
    .train_untrain_template(object, text, class, .API.untrain)
}

.API.untrain <- function(object, text, class, ...) {
    url <- paste(.base_url(), "me/", paste(object$classifier_name, class, sep="/"), "/untrain", sep="")
    content <- .to_json.text_input(text)
    response <- .POST_request(url, content, object$write_token)

    .add_response_message(object, response)
    return(.is_OK_response(response))
}

.train_untrain_template <- function(object, text, class, func, ...) {
    formatted_text <- .format.text_input(text)
    formatted_class <- .format.class_input(class)

    stopifnot(length(formatted_text$texts) == length(formatted_class))

    classes <- unique(formatted_class)

    for (class in classes) {
        if (!(.class_exists(object, class))) {
            msg <- paste(class, "does not exist.", sep=" ")
            .warn(msg)
            next
        }

        text <- formatted_text$texts[which(formatted_class == class)]

        if (!(func(object, text, class))) {
            msg <- object$cache$APImessage
            .warn(msg)
        } else {
            object$cache$dirty <- TRUE
        }
    }
}
