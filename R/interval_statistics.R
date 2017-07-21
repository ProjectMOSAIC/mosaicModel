#' Interval statistics for use with df_stats()
#' 
#' @param level Confidence level, e.g. 0.95
#' @export
coverage <- function(level = 0.95) {
  function(x, na.rm = TRUE) {
    bottom = (1 - level) / 2
    top = 1 - bottom
    res <- quantile(x, probs = c(bottom, top), 
                    na.rm = na.rm, type = 7, names = TRUE)
    names(res) <- paste("coverage", c("lower", "upper"), sep = "_")
    names(res) <- c("lower", "upper") # Do we prefer a short form?
    
    res
  }
}

#' @export
ci.mean <- function(level = 0.95) {
  level <- check.level(level)
  
  function(x, na.rm = TRUE) {
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
}

#' @export
ci.median <- function(level = 0.95) {
  level <- check.level(level)
  
  function(x, na.rm = TRUE) {
    x <- sort(x)
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    ci_index <- (n + qnorm(c(bottom, top)) * sqrt(n))/2 + c(0,1)
    res <- as.numeric(x[round(ci_index)])
    
    #names(res) <- paste("ci_median", c("lower", "upper"), sep = "_")
    names(res) <- c("lower", "upper") # Do we prefer a short form?
    res
  }
}

#' @export
ci.sd <- function(level = 0.95) {
  level <- check.level(level)

  function(x, na.rm = TRUE) {
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res = (n - 1) * 
      stats::sd(x, na.rm = TRUE)^2 /
      qchisq(c(top, bottom), df = n - 1)
    res <- sqrt(res)
    names(res) <- paste("ci_sd", c("lower", "upper"), sep = "_")
    names <- c("lower", "upper") # Do we prefer a short form?
    res 
  }
}

#' @export
proportion <- function(nm = NULL) {
  function(x) {
    if (is.null(nm)) {
      nm <- if (is.logical(x)) TRUE else unique(x)[1]
    }
  
    c(prop = mean(x == nm[1]))
  }
}

#' @export
ci.proportion <- function(nm = NULL, level = 0.95) {
  level <- check.level(level)

  function(x) {
    if (is.null(nm)) {
      nm <- if (is.logical(x)) TRUE else unique(x)[1]
    }
    
    prob <- mean(x == nm[1])
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res <- qbinom(c(bottom, top), size = length(x), prob = prob) / n
    names(res) <- paste(nm, c("lower", "upper"), sep = "_")
    
    res 
  }
} 

# internal use
check.level <- function(level) {
  res <- ifelse(level > 1, 1, ifelse(level < 0, 0, level))
  if (level < 0 | level > 1) warning("level ", level, " is not between 0 and 1. Setting to ", res)

  res
  }  
