#' Interval statistics for use with df_stats()
#' 
#' @export
coverage <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    bottom = (1 - level) / 2
    top = 1 - bottom
    res <- quantile(x, probs = c(bottom, top), 
                    na.rm = na.rm, type = 7, names = TRUE)
    names(res) <- paste("coverage", c("lower", "upper"), sep = "_")
    res
  }
}

#' @export
ci.mean <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    n <- length(x)
    lowerq <- stats::qt((1 - level) / 2, df = n - 1)
    res <- 
      base::mean(x, na.rm = na.rm) + 
        c(lowerq, -lowerq) *
        stats::sd(x, na.rm = na.rm)/sqrt(length(x))
    
    names(res) <- paste("ci_mean", c("lower", "upper"), sep = "_")
    res
  }
}

#' @export
ci.median <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    x <- sort(x)
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    ci_index <- (n + qnorm(c(bottom, top)) * sqrt(n))/2 + c(0,1)
    res <- as.numeric(x[round(ci_index)])
    
    names(res) <- paste("ci_median", c("lower", "upper"), sep = "_")
    res
  }
}

#' @export
ci.sd <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res = (n - 1) * 
      stats::sd(x, na.rm = TRUE)^2 /
      qchisq(c(top, bottom), df = n - 1)
    res <- sqrt(res)
    names(res) <- paste("ci_sd", c("lower", "upper"), sep = "_")
    res 
  }
}

