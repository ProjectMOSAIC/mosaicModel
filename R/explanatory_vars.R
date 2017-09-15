#' Get the names of the explanatory variables in a model
#' 
#' This will typically *not* be used by the end_user.
#' 
#' @param model the model in question
#' @param ... (not used)
#' 
#' @export
explanatory_vars <- function(model, ...) {
  all.vars(formula_from_mod(model)[[3]])
}

#' @export
response_var <- function(model, ...) {
  formula_from_mod(model)[[2]]
}
#' @export
response_values <- function(model, ...) { 
  eval(parse(text = response_var(model)), envir = data_from_mod(model))
}

