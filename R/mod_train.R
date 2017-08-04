#' Training interface for model architectures
#' 
#' Many model fitting functions in R such as `lm`, `glm`, and `rpart` use a dataframe/formula
#' interface, where the response and explanatory variables are contained in a single dataframe 
#' and the formula describes what variables and terms are to be included in the model. But some
#' architectures, e.g. `knn`, `lda/qda`, etc. do not adopt this convention. This function provides
#' a way to use the dataframe/formula interface for such architectures. 
#' 
#' @param architecture A character string specifying the desired architecture.
#' @param formula A formula specifying the response and explanatory variables as well as any additional
#' model terms (such as interactions).
#' @param data The dataframe to be used for fitting. 
#' @param ... Other parameters of importance for individual model architectures
#' 
#' @export
mod_train <- function(architecture = NULL, formula, data = NULL, ...) {
  architecture <- substitute(architecture)
  dots <- list(...)
  if (is.null(architecture) || inherits(architecture, "formula"))
    stop("You need to specify the model architecture.")

  this_arch <- subset(training_functions, arch == architecture)
  if (nrow(this_arch) < 1) stop("No such architecture found. Choose among ",
                                  paste("\"", this_arch$arch, "\"", collapse = ", "))
  train_fun <- eval(parse(text = paste0(this_arch$package, "::", 
                                  this_arch$fit_fun)))
  required <- this_arch$req_params
  missing_params <- ! required %in% as.character(names(dots)) # watch out for NULL in names(dots)
  if (!is.null(names(dots)) && any(missing_params)) stop("Architecture \"", architecture,
                                "\" requires these parameters", 
                                paste("\"", required[missing_params], "\"", collapse = ", "))
  
  # train_fun will set the class of the model it fits
  res <- do.call(train_fun, c(list(formula = formula, data = data), dots))

  res
}

# functions are quoted so that I don't have to worry about whether the package containing
# the function is installed on the user's system. Of course it will need to be installed
# if the user is to use the function!

training_functions <- tibble::tribble(
  ~ arch, ~fit_fun,     ~req_params,          ~package,
  "lm",             "lm",         "",          "stats",
  "logistic", "logistic",   "family",    "mosaicModel", 
  "rlm",           "rlm",         "",           "MASS",
  "rpart",       "rpart",         "",          "rpart",
  "gwm",      "gwm_fit",          "",    "mosaicModel",
  
  NA, NA, NA, NA
)



# Fit group-wise models (gwm)
gwm_fit <- function(formula, data = NULL, groups = ~ 1, mod_arch = lm, ...) {
  # figure out the grouping variables
  grouping_vars <- all.vars(groups)
  mods <- dplyr::summarise(
    dplyr::group_by_(data, grouping_vars),
    ..mod.. = list(mod_arch(formula, data, ...)))
  
  mods
}
  