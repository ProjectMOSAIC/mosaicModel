#' Extract component parts of models
#'
#' Functions for extracting data and names of explanatory and response variables from a model object
#' @rdname extract_from_model
#' @param object the model you are extracting features from.
#' @param ... additional arguments (not used)
#' @export
data_from_model <- function(object, ...) {
  UseMethod("data_from_model")
}

#' @rdname extract_from_model
explanatory_vars <- function(object, ...) {
  UseMethod("explanatory_vars")
}


explanatory_vars.Zelig <- function(object, ...) {
  all.vars(object$formula[[3]])
}

explanatory_vars.lm <-
  explanatory_vars.train <- # for caret-package models
  explanatory_vars.rpart <-
  explanatory_vars.randomForest <-
  # Need to fix this so that the items as stored in the model, 
  # e.g., as.factor(month), get returned.
  explanatory_vars.glm <- function(object, ...) all.vars(object$terms[[3]])

explanatory_vars.bootstrap_ensemble <- function(object, ...) {
  explanatory_vars(object$original_model, ...)
}
explanatory_vars.gbm <- function(object, ...) all.vars(object$Terms[[3]])
#' @rdname extract_from_model

response_var <- function(object, ...) {
  UseMethod("response_var")
}

response_var.Zelig <- function(object, ...) {
  object$formula[[2]]
}
response_var.lm <-
  response_var.train <- # for caret-package models
  response_var.rpart <-
  response_var.randomForest <-
  response_var.glm <- function(object, ...) { deparse(object$terms[[2]])}

response_var.bootstrap_ensemble <- function(object, ...) {
  response_var(object$original_model, ...)
}

response_var.gbm <- function(object, ...) { deparse(object$Terms[[2]]) }
# CHANGE THE ABOVE to draw on formula_from_mod()

response_values <- function(model) {
  Data <- data_from_model(model)
  eval(parse(text = response_var(model)), envir = Data) 
}
formula_from_mod <- function(object, ...) {
  UseMethod("formula_from_mod")
}

formula_from_mod.Zelig <- function(object, ...) {
  object$formula
}
formula_from_mod.lm <-
  formula_from_mod.rpart <-
  formula_from_mod.randomForest <-
  formula_from_mod.glm <- function(object, ...) {formula(object$terms)}
formula_from_mod.Zelig_ls <- function(object, ...) {object$formula}

formula_from_mod.gbm <- function(object, ...) {formula(object$Terms) }

#' @export
data_from_model.lm <- function(object, ...) {
  dots <- list(...)
  if ("data" %in% names(dots) && ! is.null(dots$data))
    return(dots$data)
  # The above is NOT YET IMPLEMENTED
  # When/If I add the train function ...
  # if the object has a data attribute added by train, use that
  data_in_call <- which("data" == names(object$call))
  if (length(data_in_call) == 1) {
    the_data <- eval(object$call[[data_in_call]], envir = parent.frame(3))
    if (is.data.frame(the_data)) return(the_data)
  }
}
#' @export
data_from_model.train <- data_from_model.lm # for caret-package models
#' @export  
data_from_model.glm <- data_from_model.lm
#' @export
data_from_model.randomForest <- data_from_model.lm
#' @export
data_from_model.gbm <- data_from_model.lm
#' @export
data_from_model.rpart <- data_from_model.lm
#' @export
data_from_model.bootstrap_ensemble <- function(object, ...) data_from_model(object$original_model, ...)
#' @export
data_from_model.default <- function(object, ...) {
  # A kluge for dealing with the Zelig objects. Still speculative.
  if (inherits(object, "Zelig-ls")) return(object$originaldata)
}

