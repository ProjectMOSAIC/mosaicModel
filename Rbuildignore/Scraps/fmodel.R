#' Plot out model values
#'
#' @param model A model to display graphically.
#' @param formula A formula setting the `y ~ x + color` variables.
#' @param data An (optional) data set from which to extract levels for explanatory variables.
#' @param nlevels An integer specifiying how many levels to display for variables shown at discrete levels.
#' @param at A named list giving specific values at which to hold the variables. You can accomplish
#' this without forming a list by using \code{...}. See examples.
#' @param prob_of A logical indicating whether to show probability of a given level of the output,
#'   name the class here as a character string.
#' @param intervals One of "none", "confidence", "prediction" indicating which type of interval
#'   to display.
#' @param post_transform A scalar transformation and new name for the response variable,
#' e.g. \code{post_transform = c(price = exp)} to undo a log transformation of price.
#' @param ... Specific values for explantory variables and/or arguments to predict()
#'
#' @details Often you will want to show some data along with the model functions.
#' You can do this with `ggplot2::geom_point()` making sure to set the \code{data} argument
#' to be a data frame with the cases you want to plot.
#'
#' @examples
#' \dontrun{
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' mod_plot(mod1)
#' mod_plot(mod1, ~ sector + sex + age) # not necessarily a good ordering
#' # show the data used for fitting along with the model
#' mod_plot(mod1, ~ age + sex + sector, nlevels = 8) +
#'   ggplot2::geom_point(data = mosaicData::CPS85, alpha = 0.1)
#' require(ggplot2)
#' mod_plot(mod1, ~ age + sex + sector, nlevels = 8) +
#'   geom_point(data = mosaicData::CPS85, alpha = 0.1) +
#'   ylim(0, 20)
#' mod2 <- lm(log(wage) ~ age + sex + sector, data = mosaicData::CPS85)
#' mod_plot(mod2, post_transform = c(wage = exp)) # undo the log in the display
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' mod_plot(mod3, type = "response")
#' # Adding the raw data requires an as.numeric() trick when it's TRUE/FALSE
#' mod_plot(mod3, ~ age + sex + sector, nlevels = 10, type = "response") +
#'   geom_point(data = mosaicData::CPS85,
#'   aes(x = age, y = as.numeric(married == "Married")), alpha = .1)
#' }
#' @export
#'
mod_plot <-
  function(object = NULL, model = NULL, formula = NULL, data = NULL,
           nlevels = 3, at = list(), prob_of = NULL,
           intervals = c("none", "confidence", "prediction"),
           post_transform = NULL, ...) {

    # figure out what sort of thing object is and permute other arguments as required

    add <- FALSE
    if (inherits(object, c("rpart", "glm", "lm", "groupwiseModel", "randomForest", "gbm"))) {
      if (is.null(formula) && inherits(model, "formula")) { formula <- model ; model <- object }
      if (is.null(model)) { model <- object }
    } else if (inherits(object, c("gg"))) {
      add <- TRUE
    } else if (inherits(object, c("data.frame"))) {
      if (is.null(data)) {
        data <- object
      } else {
        stop("Are you providing data twice?")
      }
    } else if (inherits(object, c("formula"))) {
      if (is.null(formula)) {
        formula <- object
      } else {
        stop("Are you providing formula twice?")
      }
    } else if (!is.null(object)) {
      stop("object has unsupported type: ", class(object)[1])
    }

  # create starting point for bulding plot layers
  if (add) {
    P <- object
  } else {
    P <- ggplot()
  }

  dots <- handle_dots_as_variables(model, ...)
  extras <- dots$extras

  # Override the values in <at> with any set as inline arguments.
  inline_values <- dots$at
  at[names(inline_values)] <- NULL
  at <- c(at, inline_values)

  intervals <- match.arg(intervals)

  if (is.null(model)) {
    stop("No model provided.")
  } else if (inherits(model,
                      c("rpart", "glm", "lm", "groupwiseModel",
                        "randomForest", "gbm", "nls"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for mod_plot().")
  }

  if( inherits(model, "gbm")) stop("gbm models still not working.")

  # If data not explicitly provided, get from model
  if (is.null(data)) data <- data_from_model(model)

  # try to figure out what are the possible levels of variables
  response_var_name <- response_var(model)
  # is the response categorical?  If so, plot the probability of the given level
  response_values <-
    if (response_var_name %in% names(data)) {data[[response_var_name]]}
  else {eval(parse(text = response_var_name), envir = data)}
  if (! inherits(response_values, c("numeric", "logical"))) {
    # It's categorical
    if (is.null(prob_of))
      prob_of <- names(sort(table(response_values), decreasing = TRUE))[1]
    if ( ! prob_of %in% response_values)
      stop("Level '", prob_of, "' doesn't exist in the response variable.")
    response_var <- prob_of
  }

  explan_vars <- explanatory_vars(model)
  if (is.null(formula)) show_vars <- explan_vars
  else show_vars <- all.vars(mosaic::rhs(formula))

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  # Use the fourth as the second <facet> variable.

  if (length(show_vars) > 4) show_vars <- show_vars[1:4]

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(Inf, rep(nlevels, length(show_vars) - 1)))
  names(how_many) <- show_vars
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )

  # set up so that glms are plotted, by default, as the response rather than the link
  if (inherits(model, "glm") && ( ! "type" %in% names(extras))) {
    extras$type = "response"
  }
  if ( ! inherits(model, c("lm", "glm", "nls")) && intervals != "none" ) {
    warning("No intervals available for model type", class(model))
    intervals = "none"
  }
  model_vals <-
    do.call(predict,c(list(model, newdata = eval_levels),
                      extras))
  if (inherits(model, "rpart") && inherits(model_vals, c("data.frame", "matrix"))) {
    # handle the matrix values from predict.rpart()
    keepers <- colnames(model_vals) == prob_of
    model_vals <- model_vals[,keepers] # just the first class
  }
  if ( ! is.null(post_transform)) model_vals <- post_transform[[1]](model_vals)

  # get the confidence or prediction intervals
  if (intervals == "none") {
    # do nothing
    Intervals <- NULL
  } else {
    if ( ! inherits(model, c("lm", "glm", "nls"))) {
      warning("Intervals not yet available for models of class ", class(model))
      Intervals <- NULL
    } else {
      Intervals <-
        do.call(predict, c(list(model, newdata = eval_levels, interval = intervals)))
      if ( ! is.null(post_transform)) {
        the_transform <- post_transform[[1]]
        for (k in 1:length(Intervals))
          Intervals[[k]] <- the_transform(Intervals[[k]])
      }
    }
  }

  # convert any quantiles for numerical levels to discrete
  first_var_quantitative <- is.numeric(eval_levels[[show_vars[1]]])
  eval_levels <- convert_to_discrete(eval_levels)
  # Deal with the response variable being a complex name
  # e.g. when it includes operations on a data variable.
  clean_response_name <- response_var_name
  if( ! response_var_name %in% names(data))
    clean_response_name <- "clean"
  eval_levels[[clean_response_name]] <- model_vals

  if ( ! is.null(Intervals))
    Intervals <- cbind(eval_levels[show_vars], data.frame(Intervals))

  # figure out the components of the plot

  if (length(show_vars) > 1 ) {
    plot_mapping <-
      aes_string(x = show_vars[1], color = show_vars[2], y = clean_response_name)
    plot_args <-
      list(data = eval_levels, inherit.aes = FALSE)
    } else {
      plot_mapping <-
        aes_string(x = show_vars[1], y = clean_response_name)
      plot_args <- list(data = eval_levels)
    }

  if (length(show_vars) == 1) {
    if (first_var_quantitative) {
      P <- P + do.call(geom_line, c(plot_args, list(mapping = plot_mapping)))
    }
    else {
      P <- P + do.call(geom_point, c(plot_args, list(mapping = plot_mapping)))
    }
  } else { # more than one explanatory variable
    if (first_var_quantitative) {
      more_mapping <- aes_string(color = show_vars[2], linetype = show_vars[2])
      more_args <- list(alpha = 0.8)
      geom <- geom_line
    } else {
      more_mapping <- aes_string(color = show_vars[2], linetype = show_vars[2])
      more_args <- list(alpha = 0.8)
      geom = geom_point
    }
    P <- P +
      do.call(geom,
              c(list(mapping = modifyList(plot_mapping, more_mapping)),
                modifyList(plot_args, more_args))
      )
  }

  # Q is another layer; let's pick a funciton to use for this layer

  if (first_var_quantitative) {
    Qgeom <- geom_ribbon
  } else {
    Qgeom <- geom_errorbar
  }

  Q <- NULL # nothing to show, unless ...
  if ( ! is.null(Intervals)) {
    if (length(show_vars) > 1) {
      Q <- Qgeom(data = Intervals,
                aes_string(x = show_vars[1],
                           y = "NULL", # don't need this
                           ymax = "upr", ymin = "lwr", fill = show_vars[2]),
                color = NA, alpha = 0.2,
                inherit.aes = FALSE)
    } else {
      Q <- Qgeom(data = Intervals,
                aes_string(x = show_vars[1],
                           y = "NULL", # don't need this
                           ymax = "upr", ymin = "lwr"),
                color = "black", alpha = 0.2, #, fill = "blue",
                inherit.aes = FALSE)
    }
  }

  if (length(show_vars) == 3)
    P <- P + facet_wrap(show_vars[3], labeller = label_both)
  if (length(show_vars) == 4)
    P <- P + facet_grid(paste(show_vars[3], "~", show_vars[4]),
                        labeller = label_both)

  if ( ! is.null(post_transform)) response_var_name <- names(post_transform)
  P <- P + ylab(response_var_name)
  P + Q # return the plot
}

#' @export
#'
mod_plot1 <- function(model=NULL, formula = NULL, data = NULL,
                   nlevels = 3, at = list(), prob_of = NULL,
                   intervals = c("none", "confidence", "prediction"),
                   post_transform = NULL, ...) {
  dots <- handle_dots_as_variables(model, ...)
  extras <- dots$extras
  inline_values <- dots$at
  # Override the values in <at> with any set as inline arguments.
  at[names(inline_values)] <- NULL
  at <- c(at, inline_values)
  intervals <- match.arg(intervals)

  if (is.null(model)) {
    stop("Must provide a model for graphing.")
  } else if (inherits(model,
                      c("rpart", "glm", "lm", "groupwiseModel",
                        "randomForest", "gbm"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for mod_plot().")
  }
  if( inherits(model, "gbm")) stop("gbm models still not working.")

  # If data not explicitly provided, get from model
  if (is.null(data)) data <- data_from_model(model)

  # try to figure out what are the possible levels of variables
  response_var_name <- response_var(model)
  # is the response categorical?  If so, plot the probability of the given level
  response_values <-
    if (response_var_name %in% names(data)) {data[[response_var_name]]}
    else {eval(parse(text = response_var_name), envir = data)}
  if (! inherits(response_values, c("numeric", "logical"))) {
    # It's categorical
    if (is.null(prob_of))
      prob_of <- names(sort(table(response_values), decreasing = TRUE))[1]
    if ( ! prob_of %in% response_values)
      stop("Level '", prob_of, "' doesn't exist in the response variable.")
    response_var <- prob_of
  }

  explan_vars <- explanatory_vars(model)
  if (is.null(formula)) show_vars <- explan_vars
  else show_vars <- all.vars(mosaic::rhs(formula))

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  # Use the fourth as the second <facet> variable.

  if (length(show_vars) > 4) show_vars <- show_vars[1:4]

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(Inf, rep(nlevels, length(show_vars) - 1)))
  names(how_many) <- show_vars
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )

  # set up so that glms are plotted, by default, as the response rather than the link
  if (inherits(model, "glm") && ( ! "type" %in% names(extras))) {
    extras$type = "response"
  }
  if ( ! inherits(model, c("lm", "glm", "nls")) && intervals != "none" ) {
    warning("No intervals available for model type", class(model))
    intervals = "none"
  }
  model_vals <-
    do.call(predict,c(list(model, newdata = eval_levels),
                      extras))
  if (inherits(model, "rpart") && inherits(model_vals, c("data.frame", "matrix"))) {
    # handle the matrix values from predict.rpart()
    keepers <- colnames(model_vals) == prob_of
    model_vals <- model_vals[,keepers] # just the first class
  }
  if ( ! is.null(post_transform)) model_vals <- post_transform[[1]](model_vals)

  # get the confidence or prediction intervals
  if (intervals == "none") {
    # do nothing
    Intervals <- NULL
  } else {
    if ( ! inherits(model, c("lm", "glm", "nls"))) {
      warning("Intervals not yet available for models of class ", class(model))
      Intervals <- NULL
    } else {
      Intervals <-
        do.call(predict, c(list(model, newdata = eval_levels, interval = intervals)))
      if ( ! is.null(post_transform)) {
        the_transform <- post_transform[[1]]
        for (k in 1:length(Intervals))
          Intervals[[k]] <- the_transform(Intervals[[k]])
      }
    }
  }

  # convert any quantiles for numerical levels to discrete
  first_var_quantitative <- is.numeric(eval_levels[[show_vars[1]]])
  eval_levels <- convert_to_discrete(eval_levels)
  # Deal with the response variable being a complex name
  # e.g. when it includes operations on a data variable.
  clean_response_name <- response_var_name
  if( ! response_var_name %in% names(data))
    clean_response_name <- "clean"
  eval_levels[[clean_response_name]] <- model_vals

  if ( ! is.null(Intervals))
    Intervals <- cbind(eval_levels[show_vars], data.frame(Intervals))

  # figure out the components of the plot

  P <-
    if (length(show_vars) > 1 ) {
      ggplot(data = eval_levels,
             aes_string(x = show_vars[1],
                        color = show_vars[2],
                        y = clean_response_name),
             group = NA)
    } else {
    ggplot(data = eval_levels,
           aes_string(x = show_vars[1],
                      y = clean_response_name),
           group = NA)
    }




  if (length(show_vars) == 1) {
    if (first_var_quantitative) {
      P <- P + geom_line()
    }
    else {
      P <- P + geom_point()
    }
  } else { # more than one explanatory variable
    if (first_var_quantitative) {

      P <- P + geom_line(
        aes_string(color = show_vars[2], linetype = show_vars[2]),
        alpha = 0.8)
    } else {
      P <- P + geom_point(
        aes_string(color = show_vars[2], linetype = show_vars[2]),
        alpha = 0.8) +
        geom_line(aes_string(group = show_vars[2], color = show_vars[2]), alpha = 0.8)
    }
  }

  if (first_var_quantitative) {
    Qfun <- geom_ribbon
  } else {
    Qfun <- geom_errorbar
  }

  Q <- NULL # nothing to show, unless ...
  if ( ! is.null(Intervals)) {
    if (length(show_vars) > 1) {
      Q <- Qfun(data = Intervals,
                aes_string(x = show_vars[1],
                           y = "NULL", # don't need this
                           ymax = "upr", ymin = "lwr", fill = show_vars[2]),
                color = NA, alpha = 0.2)
    } else {
      Q <- Qfun(data = Intervals,
                aes_string(x = show_vars[1],
                           y = "NULL", # don't need this
                           ymax = "upr", ymin = "lwr"),
                color = "black", alpha = 0.2) #, fill = "blue")
    }
  }



  if (length(show_vars) == 3)
    P <- P + facet_wrap(show_vars[3], labeller = label_both)
  if (length(show_vars) == 4)
    P <- P + facet_grid(paste(show_vars[3], "~", show_vars[4]),
                        labeller = label_both)

  if ( ! is.null(post_transform)) response_var_name <- names(post_transform)
  P <- P + ylab(response_var_name)
  P + Q # return the plot
}
