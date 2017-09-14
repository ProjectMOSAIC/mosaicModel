#' Get the name or values of the response variable from data used to train a model
#' 
#' This will typically *not* be used by the end-user.
#' 
#' @param model the model in question
#' @param ... (not used)
#' 
#' @export
response_var <- function(model, ...) {
  UseMethod("response_var")
}

response_var.default <- function(model, ...) { # works pretty broadly
  formula_from_mod(model)[[2]]
}

#' @export
response_values <- function(model, ...) {
  UseMethod("response_values")
}
response_values.default <- function(model, ...) { # works pretty broadly
  eval(parse(text = response_var(model)), envir = data_from_model(model))
}