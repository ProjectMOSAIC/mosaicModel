#' Get the names of the explanatory variables in a model
#' 
#' This will typically *not* be used by the end_user.
#' 
#' @param model the model in question
#' @param ... (not used)
#' 
#' @export
explanatory_vars <- function(model, ...) {
  UseMethod("explanatory_vars")
}

explanatory_vars.lm <-
  explanatory_vars.train <- # for caret-package models
  explanatory_vars.rpart <-
  explanatory_vars.randomForest <-
  explanatory_vars.knn3 <-
  # Need to fix this so that the items as stored in the model, 
  # e.g., as.factor(month), get returned.
  explanatory_vars.glm <- function(model, ...) all.vars(model$terms[[3]])

explanatory_vars.bootstrap_ensemble <- function(model, ...) {
  explanatory_vars(model$original_model, ...)
}
explanatory_vars.gbm <- function(model, ...) all.vars(model$Terms[[3]])


explanatory_vars.default <- function(model, ...) {
  stop("No method yet for extracting the names of the explanatory variable for models of class '", 
       paste(class(model), collapse="', "), "'.")
}

