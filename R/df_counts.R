#' Formula interface to counts
#' 
#' Counts the number of cases in a data frame broken down by the 
#' variables in the formula. Formats this in the same way as \code{df_props}.
#' @param formula the formula describing the relationship
#' @param data a data frame (or you can pipe this in)
#' @param wide reformat the output as a cross-tabulation. This makes sense only when there are just two variables
#' @param margins show the marginal probabilities. Makes the most sense if \code{wide = TRUE}.
#' @param format Used just for internal purposes.
#' 
#' @seealso \code{df_props}
#' @export
df_counts <- function(formula, data, ..., 
                      wide = FALSE, margins = FALSE) {
  df_props(formula = formula, data = data, wide = wide, margins = margins, format = "count")
}