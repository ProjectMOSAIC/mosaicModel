# Internal functions for evaluating models
# 
# These functions are the interface to the various model types for `mod_eval()`, and through 
# that to all the other `mod_` functions that need to evaluate models, e.g. `mod_effect()`, `mod_cv()`, and so on.
# 
# @param model A model object of the classes permitted
# @param data Usually, a data table specifying the inputs to the model. But if
# not specified, the training data will be used.
# @param interval One of "none", "confidence", or "prediction". Not all model
# types support "prediction" or even "confidence".
# 
# 
# @details All of the `eval_` functions are ex
# These functions return a numerical vector (for regression types) or
# a matrix of probabilities (for classifiers)
#
eval_randomForest <- function(model, data = NULL,
                              interval = c("none", "confidence", "prediction")) {
  if (!inherits(model, "randomForest")) 
    stop("model not of class randomForest")
  interval <- match.arg(interval)
  
  if (is.null(data)) data <- data_from_model(model)
  
  if (model$type == "classification") {
    res <- tibble::remove_rownames(
      as.data.frame(
        predict(model, newdata = data, type = "prob")))
  } else if (model$type == "regression") {
    res <- data.frame(model_output = 
      predict(model, newdata = data, type = "response"))
  }
  
  res
}

eval_lm <- function(model, data = NULL, interval = c("none", "confidence", "prediction")) {
  if (!inherits(model, c("lm"))) 
    stop("model not a recognized linear type of model")
  
  interval <- match.arg(interval)
  
  if (is.null(data)) data <- data_from_model(model)
  
  res <- as.data.frame(
    predict(model, newdata = data, type = "response", interval = interval )
  )
  
  if (interval == "none" || ncol(res) == 1)
    names(res) <- "model_output"
  else
    names(res) <- c("model_output", "lower", "upper")

  tibble::remove_rownames(res)
}

eval_glm <- function(model, data = NULL, interval = c("none", "confidence")) {
  if (!inherits(model, c("glm"))) 
    stop("model not a recognized logistic type of model")
  
  link_type <- model[["family"]]
  
  fun <- switch(
    link_type$link,
    "identity" = eval_lm,
    "log" = eval_poisson,
    "logit" = eval_logistic
  )
  
  fun(model = model, data = data, interval = interval)
}

eval_logistic <- function(model, data = NULL, interval = c("none", "confidence")) {
  if (!inherits(model, c("glm"))) 
    stop("model not a recognized logistic type of model")
  
  
  if ( ! base::all.equal(model[["family"]], binomial()))
    stop("incorrect family for a logistic regression")
  
  interval <- match.arg(interval)
  
  if (is.null(data)) data <- data_from_model(model) 

  se.fit <- ifelse(interval == "confidence", TRUE, FALSE)
  tmp <- predict(model, newdata = data, type = "link", se.fit = se.fit)

  if (se.fit) {
    res <- data.frame(model_output = exp(tmp$fit) / (1 + exp(tmp$fit)),
                      lower = tmp$fit - 2 * tmp$se.fit,
                      upper = tmp$fit + 2 * tmp$se.fit)
    res$lower <- exp(res$lower) / (1 + exp(res$lower))
    res$upper <- exp(res$upper) / (1 + exp(res$upper))
  } else {
    names(tmp) <- NULL # strip off case labels
    res <- data.frame(model_output = exp(tmp) / (1 + exp(tmp)))
  }
  
  
  tibble::remove_rownames(res)
}

eval_poisson <- function(model, data = NULL, interval = c("none", "confidence")) {
  if (!inherits(model, c("glm"))) 
    stop("model not a recognized poisson type of model")

  if ( ! base::all.equal(model[["family"]], poisson()))
    stop("incorrect family for a poisson regression")
  
  interval <- match.arg(interval)
  
  if (is.null(data)) data <- data_from_model(model) 
  
  se.fit <- ifelse(interval == "confidence", TRUE, FALSE)
  tmp <- predict(model, newdata = data, se.fit = se.fit, type = "link")
     
  if (se.fit) {
    res <- data.frame(model_output = exp(tmp$fit),
                      lower = tmp$fit - 2 * tmp$se.fit,
                      upper = tmp$fit + 2 * tmp$se.fit)
    res$lower <- exp(res$lower)
    res$upper <- exp(res$upper)
  } else {
    names(tmp) <- NULL # strip off case labels
    res <- data.frame(model_output = exp(tmp))
  }
  
  tibble::remove_rownames(res)
}

eval_rpart <- function(model, data = NULL, interval = "none") {
  if (!inherits(model, c("rpart"))) 
    stop("model not a recognized rpart type of model")
  
  interval <- match.arg(interval)
  
  if (is.null(data)) data <- data_from_model(model) 
  
  if (model$method == "class") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob" )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data)
    )
    names(res) <- "model_output"
  }
  
  tibble::remove_rownames(res)
}


# FOR K-nearest neighbors