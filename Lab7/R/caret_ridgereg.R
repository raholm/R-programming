#' Ridge Regression caret model
#'
#' A caret model of the ridge regression
#'
#' @export
cridgereg <- list(label="Ridge Regression",
                  type="Regression",
                  library="Lab7",
                  parameters=data.frame(parameter="lambda",
                                        class="numeric",
                                        label="lambda"),
                  grid=function(x, y, len=null, search="grid") {
                      if (is.null(len)) len <- 2
                      grid <- expand.grid(lambda = seq(0, len, by=0.25))
                  },
                  fit=function(x, y, wts, param, lev, last, classProbs, ...) {
                      data <- as.data.frame(x)
                      data$y <- y

                      ## print(names(data))
                      mod <- ridgereg(y ~ ., data=data, lambda=param$lambda)
                      mod
                  },
                  predict=function(modelFit, newdata, submodels=NULL) {
                      return(modelFit$pred(newdata))
                  },
                  loop=NULL,
                  sort=NULL,
                  prob=NULL
                  )
