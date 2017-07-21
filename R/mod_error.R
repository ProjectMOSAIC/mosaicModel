#' Mean square prediction error
#'
#' Compares model predictions to the actual value of the response variable.
#' To do this, testing data must be provided with *both* the input variables and the 
#' corresponding response variable. The measure calculated for a quantitative response 
#' variable is the mean square prediction error (MSPE). 
#' For categorical response variables, an analog of MSPE can be calculated (see details) 
#' but by default, a mean log-likelihood (mean per case) is computed instead.
#' 
#' @param model The model whose prediction error is to be estimated.
#' @param testdata A data frame giving both model inputs and the actual value of the response
#' variable. If no testing data is provided, the training data will be used and a warning issued.
#' @param SS Flag to have output be the sum of square errors.
#' @param LL Flag to calculate log likelihood when the response variable is categorical. 
#' 
#' @details When the response variable is categorical, the model 
#' (called a 'classifier' in such situations) must be capable of 
#' computing *probabilities* for each output rather than just a bare category. 
#' This is true for many commonly
#' encountered model architectures.
#' 
#' The analog of the square prediction value for classifiers is (1-p)^2, where p is the
#' probability assigned by the model to the actual output. This is a rough approximation 
#' to the log-likelihood. By default, the log-likelihood will be calculated, but for pedagogical
#' reasons you may prefer (1-p)^2, in which case set `LL = FALSE`.
#' 
#' @examples
#' mod <- lm(mpg ~ hp + wt, data = mtcars)
#' mod_error(mod) # In-sample prediction error.
#' \dontrun{
#' classifier <- rpart::rpart(Species ~ ., data = iris)
#' mod_error(classifier)
#' mod_error(classifier, LL = FALSE) 
#' # More typically
#' inds <- sample(1:nrow(iris), size = 100)
#' Training <- iris[inds, ]
#' Testing  <- iris[ - inds, ]
#' classifier <- rpart::rpart(Species ~ ., data = Training)
#' # This may well assign zero probability to events that appeared in the
#' # Testing data 
#' mod_error(classifier, testdata = Testing)
#' mod_error(classifier, testdata = Testing, LL = FALSE)
#' }
#' @export
mod_error <- function(model, testdata, SS = FALSE, LL = TRUE) {
  if (missing(testdata)) {
    testdata <- data_from_model(model)
    warning("Calculating error from training data.")
  }
  # mosaic::MSPE(mod, testdata, LL = LL)
  # Needed to change code, but didn't update MSPE in mosaic package
  # This is re-written to use mod_eval() rather than predict()
  actual <- eval(parse(text = response_var(model)), envir = testdata)
  model_vals <- mod_eval(model, data = testdata, append = FALSE)
  if (is.numeric(actual)) {
    fun <- ifelse(SS, base::sum, base::mean)
    res <- fun((actual - model_vals)^2, na.rm = TRUE)
  }
  else {
    probs <- p_of_actual(model_vals, actual)
    res <- if (LL) {
      - mean(log(probs))
    }
    else {
      res <- stats::var(1 - probs, na.rm = TRUE)
    }
  }
  res
}

# Compute the probability corresponding to the actual categorical
# result
p_of_actual <- function(probs, actual_value) {
  res <- numeric(nrow(probs))
  possibilities <- gsub("^model_output.", "", names(probs))
  for (k in 1:length(possibilities)) {
    inds <- actual_value == possibilities[k]
    res[inds] <- probs[inds, k]
  }
  
  res
}
