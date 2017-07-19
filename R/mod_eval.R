#' Evaluate a model for specified inputs
#' 
#' Find the model outputs for specified inputs. This is equivalent to the 
#' generic \code{predict()} function, except it will choose sensible values
#' by default.  This simplifies getting a quick look at model values.
#' 
#' @return A dataframe containing both the explanatory variable inputs and
#' the resulting model output (in the `model_value` field). This differs from the output
#' of \code{predict()}, which for many model classes/architectures may be a vector or matrix.
#'
#' @importFrom stats formula lm median model.matrix predict quantile sd
#' @importFrom ggplot2 aes_string facet_grid facet_wrap geom_errorbar geom_line geom_point geom_ribbon ggplot label_both ylab
#' @importFrom utils data
#' 
#' @param model the model to display graphically
#' @param data optional set of cases from which to extract levels for explanatory variables
#' @param on_training flag whether to use the training data for evaluation. Only needed
#' when there are random terms, e.g. from \code{rand()}, \code{shuffle()}, .... See details.
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. \code{pretty()} will refine your choice. (default: 3) 
#' @param ... arguments about or values at which to evaluate the model or the kind of output to be passed along to predict().
#' Unlike \code{data =} the variables given in \code{at =} or \code{...} will be crossed, so that
#' the evaluation will occur at all combinations of the various levels.
#' @param append flag whether to include the inputs to the model along with the calculated
#' model value in the output. Default: \code{TRUE}. 
#' @return A data frame containing both the inputs to the model and the corresponding outputs.
#'
#' @details There are four distinct ways to specify the values at which the model is to be evaluated.
#' (1) Look for some "typical values" in the data to create a handful of inputs. This is useful for 
#' getting a quick look at what the output of the model looks like. This is the default behavior. (2) Using \code{data =} to a dataframe containing the explanatory variables will evaluate the model
#' at all of the cases contained in that dataframe. (3) Setting input variables explicitly by using 
#' arguments of the form var_name = values, e.g. `sex = "F"`. If not all input variables are specified in 
#' this way, the ones that are not will have values set per (1). All combinations of the various variables
#' will be created. See the \code{nlevels} argument. (4) Evaluating the model on the training data.
#' There are two ways to do this. The first is
#' to set the \code{data} argument to the same data frame used to train the model. The second
#' is to use the \code{on_training = TRUE} argument. These are equivalent unless there is
#' some random component among the explanatory terms, as with `mosaic::rand()`, `mosaic::shuffle()` and so on.
#'  
#'
#' @examples
#' \dontrun{mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' mod_eval(mod1)
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' mod_eval(mod3, nlevels = 2, type = "response")
#' mod_eval(mod3, nlevels = 2, type = "response", sex = "F") 
#' at = list(sex = "F"))
#' }
#' @export
mod_eval <- function(model = NULL, data = NULL, append = TRUE, intervals = c("none", "prediction", "confidence"),
                   nlevels = 3, ..., on_training = FALSE) {
  dots <- handle_dots_as_variables(model, ...)
  extras <- dots$extras
  at <- dots$at
  
  intervals <- match.arg(intervals)
  # Override the values in <at> with any set as inline arguments.
  #at[names(inline_values)] <- NULL
  #at <- c(at, inline_values)
  
  if (is.null(model)) {
    stop("Must provide a model to evaluate.")
  } else if (inherits(model, 
                      c("rpart", "glm", "lm", "groupwiseModel",
                        "randomForest", "gbm"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for mod_evaluate().")
  }
  if( inherits(model, "gbm")) stop("gbm models still not working.")
  
  eval_levels <- 
    if (on_training) {
      data_from_model(model)
    } else {
      if (is.null(data)) {
        typical_levels(model = model, data = data, nlevels = nlevels, at = at)
      } else {
        data
      }
    }
  
  # need a more sophisticated model than this, but for now ...
  predfun <- predict
  if (inherits(model, "gam")) predfun <- predict.glm
  # then add others for the sorts of model types where predict() has
  # a nasty interface
  
  model_vals <- 
    if (on_training) { 
      do.call(predfun, c(list(model), extras))
    } else {
        do.call(predfun,c(list(model, newdata = eval_levels), extras))
    }
  
  if (append) {
    output <- eval_levels
    output$model_output <- model_vals
  }
  else output = data.frame(model_output = model_vals) 
    
  output
}

