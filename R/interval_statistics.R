#' Interval statistics
#' 
#' Calculate coverage intervals and confidence intervals for the sample mean, median, sd, proportion, ...
#' 
#' Typically, these will be used within `df_stats()`. For the mean, median, and sd, the variable x must be 
#' quantitative. For proportions, the x can be anything; use the `prop_of` argument to specify what 
#' value you want the proportion of. Default for `prop_of` is `TRUE` for x logical, or the first level returned
#' by `unique` for categorical or numerical variables.  The CI for proportions is computed using quantiles of the
#' binomial distribution.
#'
#' @param x A variable.
#' @param prop_of For proportions, this specifies the categorical level for which the calculation of proportion will
#' be done. Defaults: `TRUE` for logicals for which the proportion is to
#' be calculated.
#' @param level Number in 0 to 1 specifying the confidence level for the interval. (Default: 0.95)
#' @param na.rm if `TRUE` disregard missing data 
#' 
#' @rdname intervals
#' @aliases coverage ci.mean ci.median ci.sd ci.proportion 
#' 
#' @examples
#' # The central 95% interval
#' df_stats(hp ~ cyl, data = mtcars, c95 = coverage(0.95))
#' # The confidence interval on the mean
#' df_stats(hp ~ cyl, data = mtcars, mean, ci.mean)
#' # What fraction of cars have 6 cylinders?
#' df_stats(mtcars, ~ cyl, six_cyl_prop = ci.proportion(prop_of = 6, level = 0.90))
#' 
#' @export
coverage <- function(x, level = 0.95, na.rm = TRUE) {
  if (! is.numeric(x)) stop("The variable x must be quantitative.")
  level <- check.level(level)
  bottom = (1 - level) / 2
  top = 1 - bottom
  res <- quantile(x, probs = c(bottom, top), 
                  na.rm = na.rm, type = 7, names = TRUE)
  names(res) <- paste("coverage", c("lower", "upper"), sep = "_")
  names(res) <- c("lower", "upper") # Do we prefer a short form?
  
  res
}

#' @export
ci.mean <- function(x, level = 0.95, na.rm = TRUE) {
  if (! is.numeric(x)) stop("The variable x must be quantitative.")
  level <- check.level(level)
  
  n <- length(x)
  lowerq <- stats::qt((1 - level) / 2, df = n - 1)
  res <- 
    base::mean(x, na.rm = na.rm) + 
    c(lowerq, -lowerq) *
    stats::sd(x, na.rm = na.rm)/sqrt(length(x))
  
  # names(res) <- paste("ci_mean", c("lower", "upper"), sep = "_")
  names(res) <- c("lower", "upper") # Do we prefer a short form?
  
  res
}

#' @export
ci.median <- function(x, level = 0.9, na.rm = TRUE5) {
  if (! is.numeric(x)) stop("The variable x must be quantitative.")
  level <- check.level(level)
  x <- sort(x)
  n <- length(x)
  bottom <- (1 - level) / 2
  top <- 1 - bottom
  ci_index <- (n + qnorm(c(bottom, top)) * sqrt(n))/2 + c(0,1)
  res <- as.numeric(x[round(ci_index)])
  
  names(res) <- c("lower", "upper") 
  res
}


#' @export
ci.sd <- function(x, level = 0.95, na.rm = TRUE) {
  if (! is.numeric(x)) stop("The variable x must be quantitative.")
  level <- check.level(level)
  n <- length(x)
  bottom <- (1 - level) / 2
  top <- 1 - bottom
  res = (n - 1) * 
    stats::sd(x, na.rm = TRUE)^2 /
    qchisq(c(top, bottom), df = n - 1)
  res <- sqrt(res)
  #names(res) <- paste("ci_sd", c("lower", "upper"), sep = "_")
  names(res) <- c("lower", "upper") 
  res 
}

#' @export
ci.proportion <- function(x,  prop_of = NULL, level = 0.95) {
  level <- check.level(level)
  
  nm = prop_of
  if (is.null(nm)) {
    nm <- if (is.logical(x)) TRUE else unique(x)[1]
  }
  
  prob <- mean(x == nm[1])
  n <- length(x)
  bottom <- (1 - level) / 2
  top <- 1 - bottom
  res <- qbinom(c(bottom, top), size = length(x), prob = prob) / n
  res[3] <- res[2]
  res[2] <- prob
  names(res) <- c("lower", "center", "upper")
  res 
}

# internal use
check.level <- function(level) {
  res <- ifelse(level > 1, 1, ifelse(level < 0, 0, level))
  if (level < 0 | level > 1) warning("level ", level, " is not between 0 and 1. Setting to ", res)

  res
}  
