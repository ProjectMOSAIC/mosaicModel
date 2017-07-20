#' Find typical levels of explanatory variables in a model/dataset.
#' 
#' This function tries to choose sensible values of the explanatory variables 
#' from the data used to build a model or any other specified data. 
#' (or from data specified with the \code{data =} argument.)
#' 
#' @details For categorical variables, the most populated levels are used. For quantitative 
#' variables, a sequence of \code{pretty()} values is generated. 
#' 
#' @return A dataframe containing all combinations of the selected values for 
#' the explanatory variables. If there are p explanatory variables, 
#' there will be about \code{nlevels^p} cases.
#'
#' @param data optional data frame from which to extract levels for explanatory variables
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. Set to `Inf` to get all levels 
#' for categorical variables and 100 levels for quantitative variables.
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables.
#' @param model the model to display graphically
#' @param ... a more concise mechanism to passing desired values for variables
#'
#' @details For categorical variables, will return the nlevels most popular levels, unless 
#' the levels are specified explicitly in an argument.
#'
#' @examples
#' \dontrun{
#' df_typical(mosaicData::Galton, nlevels = 2, father = 70, mother = 68, nkids = 3)
#' df_typical(mosaicData::Galton, nlevels = 2)
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' df_typical(model = mod1, nlevels = 3)
#' }
#' @export
df_typical <- function(data = NULL,  
                   nlevels = 3, at = list(), model=NULL, ...) {
  extras <- list(...)
  at <- c(extras, at)
  
  # try to figure out what are the possible levels of variables
  if ( (! is.null(model)) && is.null(data)) data <- data_from_model(model)
  explan_vars <- if (! is.null(model)) explanatory_vars(model) else names(data)

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(rep(nlevels, length(explan_vars))))
  names(how_many) <- explan_vars
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )
  vnames <- names(eval_levels)
  for (name in vnames) {
    if (inherits(data[[name]], "factor") && !inherits(eval_levels[[name]], "factor"))
      eval_levels[[name]] <- factor(eval_levels[[name]], levels = levels(data[[name]]))
  }

  eval_levels
}

