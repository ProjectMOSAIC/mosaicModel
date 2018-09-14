#' Evaluate a model for specified inputs
#'
#' Find the model outputs for specified inputs. This is equivalent to the
#' generic `predict()` function, except it will choose sensible values
#' by default.  This simplifies getting a quick look at model values.
#'
#' @return A dataframe containing both the explanatory variable inputs and
#' the resulting model output (in the `model_value` field). This differs from the output
#' of `stats::predict()`, which for many model classes/architectures may be a vector or matrix.
#'
#' @importFrom stats formula lm median model.matrix predict quantile sd binomial poisson qbinom qchisq qnorm
#' @importFrom utils data
#' @importFrom ggplot2 aes_string facet_wrap facet_grid geom_errorbar ylab geom_line label_both geom_point geom_ribbon ggplot
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom lazyeval lazy_dots
#' @importFrom splines ns
#' @importFrom mosaicCore df_stats
#'
#' @param model The model to display graphically
#' @param data Optional set of cases from which to extract levels for explanatory variables
#' @param on_training A logical indicating whether to use the training data for evaluation. Only needed
#' when there are random terms, e.g. from `rand()`, `shuffle()`, .... See details.
#' @param interval The type of interval to use: "none", "confidence", "prediction". But not all
#' types are available for all model architectures.
#' A standard error and confidence interval are produced in the output
#' @param bootstrap If > 1, the number of bootstrap trials to run to construct a
#' standard error on the model output for each value of the inputs. This is an alternative to
#' `interval`; you can't use both.
#' @param nlevels An integer indicating how many levels to construct for input variables. (default: 3)
#' For quantitative variables, this is a suggestion; an attempt is made to have the levels equally spaced. If you're dissatisfied
#' with the result, use  `...` to specify exactly what levels you want for any variable.
#' @param level Confidence level. Ignored for models that don't produce confidence intervals.
#' @param ... Arguments about or values at which to evaluate the model or the kind of output to be passed along to predict().
#'   Unlike `data =` the variables given in `at =` or `...` will be crossed, so that
#'   the evaluation will occur at all combinations of the various levels.
#' @param append A logical indicating whether to include the inputs to the model along with the calculated
#' model value in the output. Default: `TRUE`.
#' @return A data frame containing both the
#' inputs to the model and the corresponding outputs.
#'
#' @details There are four distinct ways to specify the values at which the model is to be evaluated.
#' (1) Look for some "typical values" in the data to create a handful of inputs. This is useful for
#' getting a quick look at what the output of the model looks like. This is the default behavior. (2) Using \code{data =} to a dataframe containing the explanatory variables will evaluate the model
#' at all of the cases contained in that dataframe. (3) Setting input variables explicitly by using
#' arguments of the form var_name = values, e.g. `sex = "F"`. If not all input variables are specified in
#' this way, the ones that are not will have values set per (1). All combinations of the various variables
#' will be created. See the `nlevels` argument. (4) Evaluating the model on the training data.
#' There are two ways to do this. The first is
#' to set the `data` argument to the same data frame used to train the model. The second
#' is to use the `on_training = TRUE` argument. These are equivalent unless there is
#' some random component among the explanatory terms, as with `mosaic::rand()`, `mosaic::shuffle()` and so on.
#'
#'
#' @examples
#' \dontrun{
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' mod_eval(mod1)
#' mod_eval(mod1, interval = "prediction") # 95% confidence level
#' mod_eval(mod1, interval = "prediction", level = 0.9)
#' mod2 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' mod_eval(mod2, nlevels = 2)
#' mod_eval(mod2, nlevels = 2, sex = "F")
#' mod_eval(mod2, nlevels = 2, sex = "F",
#'   interval = "confidence", level = 0.9)
#' }
#' @export
mod_eval <-
  function(
    model = NULL, data = NULL, append = TRUE,
    interval = c("none", "prediction", "confidence"),
    nlevels = 2, bootstrap = 0, level = 0.95, ...,
    on_training = FALSE) {

  interval <- match.arg(interval)

  eval_levels <-
    if (on_training) {
      data_from_mod(model)
    } else { # If no data provided, get typical levels
      if (is.null(data)) {
        dots <- handle_dots_as_variables(model, ...)
        extras <- dots$extras
        at <- dots$at
        df_typical(model = model, data = data, nlevels = nlevels, at = at)
      } else {
        data
      }
    }

  if (bootstrap > 1) {
    sofar <- NULL
    ensemble <- mod_ensemble(model, bootstrap)
    Bootstrap_reps <-
      mod_eval(ensemble, data=eval_levels, append = FALSE,
               interval = "none", on_training = on_training, ...)
    res <- Bootstrap_reps %>%
      dplyr::group_by(.trial) %>%
      dplyr::mutate(.row = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.row) %>%
      dplyr::select(-.trial)
    res <- res %>%
      dplyr::summarise_all(dplyr::funs(mn = mean,
                      se = sd), na.rm=TRUE) %>%
      dplyr::select(-.row)
    if (names(res)[2] == "se") # handle naming for regression case
      names(res) <- c("model_output", "model_output_se")
    nclasses <- ncol(res) / 2
    if (nclasses > 1) # reorder multiple columns nicely into mean, sd for each class
      res <- res[,rep(c(0,nclasses), nclasses) + rep(1:nclasses, each = 2)]
    names(res) <- gsub("_mn$","", names(res))
    if (append) res <- dplyr::bind_cols(eval_levels, res)

    return(res)
  }

  if (is.null(model)) {
    stop("Must provide a model to evaluate.")
  }

  if (inherits(model, "bootstrap_ensemble")) {
    nreps <- length(model$replications)
    output <- as.list(numeric(nreps))
    for (k in 1:nreps) {
      model_vals <-
        mod_output(model$replications[[k]], data = eval_levels, interval = interval, level = level)
      if (append) output[[k]] <- dplyr::bind_cols(eval_levels, model_vals)
      else output[[k]] <- model_vals

      output[[k]][[".trial"]] <- k
    }
    output <- dplyr::bind_rows(output)
  } else {
    model_vals <-
      mod_output(model, data = eval_levels, interval = interval, level = level)

    if (append) output <- dplyr::bind_cols(eval_levels, model_vals)
    else  output <- model_vals
  }

  output
}

#' @export
#' @rdname mod_eval
#'
mod_output <-
  function(model = NULL, data = NULL, append = TRUE,
           interval = c("none", "prediction", "confidence"),
           nlevels = 2, bootstrap = 0, ..., on_training = FALSE) {

    mod_eval(
      model = model, data = data, append = append,
      interval = interval,
      nlevels = nlevels, bootstrap = bootstrap, ...,
      on_training = on_training) # [["model_output"]] taken out 9/13/18 by DTK

}
