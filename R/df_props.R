#' Joint and conditional proportions
#' 
#' Uses a formula interface to specify how the proportions are to be calculated.

#' @details
#' Using `|` in the formula specifies a conditional proportion
#' * ~ A : proportion of cases in each level of A
#' * ~ A + B: joint proportion: each level of A crossed with B
#' * ~ A | B: conditional proportion: for each level of B, what fraction are in each level of A
#' * A ~ B: another way of specifying the conditional proportion
#' 
#' 
#' @param formula the formula describing the relationship
#' @param data a data frame (or you can pipe this in)
#' @param as.percent show proportions in percent (e.g. multiply by 100)
#' @param wide reformat the output as a cross-tabulation. This makes sense only when there are just two variables
#' @param margins show the marginal probabilities. Makes the most sense if \code{wide = TRUE}.
#' @param format Use just for internal purposes.
#' 
#' @examples
#' mtcars %>% df_props( ~ cyl + gear) 
#' mtcars %>% df_props( ~ cyl | gear)
#' mtcars %>% df_props( ~ cyl + gear, wide = TRUE)
#' mtcars %>% df_props( ~ cyl + gear, margins = TRUE)
#' mtcars %>% df_props( ~ cyl | gear, margins = TRUE)
#' 
#' @export
df_props <- function (formula, data, as.percent = FALSE, ..., 
                      wide = FALSE, margins = FALSE, format = c("proportion", "percent", "count")) 
{
  if (inherits(formula, "data.frame") && inherits(data, "formula")) {
    Tmp <- data
    data <- formula
    formula <- Tmp
  }
  if (margins & !wide) {
    wide <- TRUE
    warning("Switching to wide mode to display marginals.")
  }
  format <- match.arg(format)
  fmt <- ifelse(as.percent, "percent", format)
  Tmp <- mosaic::tally(formula, data, format = fmt, ..., margins = margins)
  Tmp <- as.data.frame(Tmp)
  names(Tmp)[ncol(Tmp)] <- fmt
  
  if (wide) {
    if (ncol(Tmp) == 3) {
      Tmp <- tidyr::spread_(Tmp, 
                            key = names(Tmp)[2], names(Tmp)[3],
                            sep = "_")
    } else if (ncol(Tmp) == 2) {
      Tmp <- tidyr::spread_(Tmp, 
                            key = names(Tmp)[1], names(Tmp)[2],
                            sep = "_")
    }
    else warning("Wide output makes sense only with two variables in formula.")
  } else {
    Tmp <- group_by_(Tmp, names(Tmp)[1])
  }
  
  attributes(Tmp) <- c(attributes(Tmp), formula = formula)
  Tmp
}


