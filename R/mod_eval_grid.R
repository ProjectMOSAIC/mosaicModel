#' Create discrete levels of explanatory variables in a model/dataset.
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
#' @param mod either a data frame or a model from which to extract the data to be discretized. If
#' a model is given, then the model output will be appended for all rows.
#' @param formula a formula with one variable to identify a variable to be sampled finely (npts over the range)
#' @param nlevels the number of discrete levels to use, by default
#' @param pretty if TRUE, make the discretized versions of numerical values sensibly spaced for viewing
#' @param npts the number of points to evaluate the variable identified by formula (default: 100)
#' @param ... a more concise mechanism to passing desired values for variables
#'
#' @details For categorical variables, will return the nlevels most popular levels, unless
#' the levels are specified explicitly in an argument. When the model is a classifier, the model outputs will
#' be the probabilities of each level in the response variable. These will be named `prob_[level]`.
#'
#' Using `pretty = TRUE` may cause the number of levels for quantitative variables to be somewhat different
#' from `nlevels`.
#'
#' @examples
#' \dontrun{
#' mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' for_plotting <- mod_eval_grid(mod1, ~ age, nlevels = 4)
#' gf_line(model_output ~ age | sector, color = ~ sex, data = for_plotting ) %>%
#'   gf_point(wage ~ age | sector, color = ~ sex, data = CPS85)
#' for_plotting2 <- mod_eval_grid(mod1, ~ age, sector = c("const", "service"))
#' gf_line(model_output ~ age | sector, color = ~ sex, data = for_plotting2 )
#' }
#' @export
mod_eval_grid <- function(mod, formula = NULL, nlevels = 3, pretty = TRUE, npts = 100, ...) {
  specifics <- list(...)
  # collect the variables to be sampled finely
  fine_vars <-
    if ( ! is.null(formula)) {
      stopifnot(length(formula) == 2)
      all.vars(formula[[2]])
    } else {
      NULL
    }

  # get the explanatory var data from the model
  data <- data_from_mod(mod)
  data <- data[explanatory_vars(mod)]

  discrete_levels <- list()
  quantiles <- seq(.05, .95, length = nlevels)
  for (var_name in names(data)) {
    if (var_name %in% names(specifics)) {
      discrete_levels[[var_name]] <- specifics[[var_name]] # get values from the ... list
    } else {
      var_value <- data[[var_name]]
      if (is.numeric(var_value)) {
        discrete_levels[[var_name]] <-
          if (var_name %in% fine_vars) {
            seq(min(var_value), max(var_value), length = npts)
          } else {
            ifelse(pretty, base::pretty, I)(quantile(var_value, quantiles, na.rm = TRUE))
          }
      } else {
        # pull out the most popular levels: try to maintain the type of the variable (e.g. factor, char, ...)
        levels <- sort(unique(var_value))
        counts <- table(var_value)
        keepers <- order(counts, decreasing = TRUE)
        if (nlevels < length(keepers) && (! var_name %in% fine_vars))
          keepers <- keepers[1:nlevels]
        discrete_levels[[var_name]] <- levels[keepers]
      }
    }
  }

  res <- expand.grid(discrete_levels, stringsAsFactors = FALSE)
  mod_eval(mod, data = res)
}


