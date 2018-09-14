#' Internal functions for evaluating models
#'
#' These functions are the interface to the various model types for `mod_eval()`, and through
#' that to all the other `mod_` functions that need to evaluate models, e.g. `mod_effect()`, `mod_cv()`, and so on.
#'
#' @param model A model object of the classes permitted
#' @param data Usually, a data table specifying the inputs to the model. But if
#'   not specified, the training data will be used.
#' @param interval One of "none", "confidence", or "prediction". Not all model
#'   types support "prediction" or even "confidence".
#' @param level Confidence level for intervals.  Ignored for models that don't provide confidence
#'   intervals.
#' @param ... Additional arguments, typically passed through to an underlying `predict()` method.
#'
#'
#' @details
#' These functions return a data frame with one numerical vector (for regression types) or
#' a matrix of probabilities (for classifiers)
#'
#' @export
mod_output <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  UseMethod("mod_output")
}
#' @export
mod_output.default <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  stop("The modelMosaic package doesn't have access to an evaluation function for this kind of model object.")
}
#' @export
mod_output.lm <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  interval <- match.arg(interval, c("none", "confidence", "prediction"))

  if (is.null(data)) data <- data_from_mod(model)

  res <- as.data.frame(
    predict(model, newdata = data, type = "response",
            interval = interval, level = level, ...)
  )

  if (interval == "none" || ncol(res) == 1)
    names(res) <- "model_output"
  else
    names(res) <- c("model_output", "lower", "upper")

  tibble::remove_rownames(res)
}

#' @export
mod_output.randomForest <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  interval <- match.arg(interval,
                        choices = c("none", "confidence", "prediction"))

  if (is.null(data)) data <- data_from_mod(model)

  if (model$type == "classification") {
    res <- tibble::remove_rownames(
      as.data.frame(
        predict(model, newdata = data, type = "prob", level = level, ...)))
  } else if (model$type == "regression") {
    res <- data.frame(model_output =
      predict(model, newdata = data, type = "response", level = level, ...))
  }

  res
}

#' @export
mod_output.glm <-
  function(model, data = NULL, interval = c("none", "confidence"),
           level = 0.95, ...) {
  interval <- match.arg(interval)

  if (is.null(data)) data <- data_from_mod(model)

  vals <- predict(model, newdata = data,
                  type = "link", se.fit = interval == "confidence", ...)

  if (interval == "confidence") {
    alpha <- 0.5 * (1 - level)
    z.star <- stats::qnorm(1 - alpha)

    res <- data.frame(model_output = model$family$linkinv(vals$fit),
                      lower = vals$fit - z.star * vals$se.fit,
                      upper = vals$fit + z.star * vals$se.fit)
    res$lower <- model$family$linkinv(res$lower)
    res$upper <- model$family$linkinv(res$upper)
  } else {
    names(vals) <- NULL # strip off case labels
    res <- data.frame(model_output = model$family$linkinv(vals))
  }

  tibble::remove_rownames(res)
}

#' @export
mod_output.rpart <- function(model, data = NULL, interval = "none", level = level, ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- data_from_mod(model)

  if (model$method == "class") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ...)
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data)
    )
    names(res) <- "model_output"
  }

  tibble::remove_rownames(res)
}

#' @export
mod_output.randomForest <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- data_from_mod(model)

  if (model$type == "classification") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ...)
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data, ...)
    )
    names(res) <- "model_output"
  }

  tibble::remove_rownames(res)
}

#' @export
mod_output.knn3 <- function(model, data = NULL, interval = "none", level = level, ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- data_from_mod(model)

  res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ...)
  )

  tibble::remove_rownames(res)
}

#' @export
mod_output.train <- function(model, data = NULL, interval = "none", level = 0.95, ...) { # caret-package
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- data_from_mod(model)

  if (model$modelInfo$type[1] == "Regression") {
    res <- as.data.frame(
      predict(model, newdata = data, type = "raw", ...)
    )
    names(res) <- "model_output"
  } else if (model$modelInfo$type[1] == "Classification") {
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ...))
  } else {
    stop("Caret model is neither classifier nor regression. mosaicModel doesn't know what to do.")
  }

  tibble::remove_rownames(res)
}

#' @export
mod_output.lda <- function(model, data = NULL, interval = "none", level = 0.95, ...) {
  if (is.null(data)) data <- data_from_mod(model)

  res <- as.data.frame(predict(model, newdata = data, ...)$posterior)

  tibble::remove_rownames(res)
}
#' @export
mod_output.qda <- mod_output.lda

#' @export
mod_output.nls <- function(model, data = NULL, interval = "none", level = level, ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- data_from_mod(model)

  res <- as.data.frame(
    predict(model, newdata = data, ...)
  )

  tibble::remove_rownames(res)
}
