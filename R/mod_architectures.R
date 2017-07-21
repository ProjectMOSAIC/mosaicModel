#' The model architectures supported in `mosaicModel`
#' 
#' Identifies the available model architectures so that you can ask for ones that 
#' aren't yet there! Even better, for those with the needed R skills,
#' if you can create the necessary infrastructure and
#' put it in a pull request to the repo identified in the `DESCRIPTION` file for the package.

#' @rdname mod_architectures
#' 
#' @details The information about the supported architectures is in
#' an internal object called `architectures`. End-users will not need this object. They just have to 
#' know what model architectures are supported.
#' TO ADD A NEW ARCHITECTURE
#' 1. Create the appropriate eval_ function
#' 2. Either register the model class and eval_ function in the `architectures` table above
#'     or add it to an oversight eval_ fucntion such as eval_glm
#' 3. Create a data_from_model() method for the new class. If it's not possible to extract 
#'     the data from the model object, throw an error in your method.
#' 4. Similarly, create methods for explanatory_vars(), response_var(), 
#'     response_values(), and formula_from_model()

#' @export
mod_architectures <- function() {
  return(architectures$mod_class)
}

architectures <- tibble::tribble(
  ~mod_class, ~eval_fun, ~has_data,
  # the order matters
  "glm",      eval_glm, data_from_model.lm,
  "lm",       eval_lm, data_from_model.lm,
  "rlm",      eval_lm, data_from_model.lm, 
  "rpart",    eval_rpart, data_from_model.rpart,
  "randomForest", eval_randomForest, data_from_model.randomForest
  
)



get_eval_function <- function(model) {
  # find the ones that match
  inds <- which(architectures$mod_class %in% class(model))
  if (length(inds) == 0) 
    stop("No mosaicModel evaluation function found for a model of class ", 
         paste('"', class(model), '"', collapse = ", "))
  
  # return the earliest function in the architecture list and other information
  res <- list(eval_fun = architectures$eval_fun[[min(inds)]],
              has_data = architectures$has_data[[min(inds)]])
  res$intervals <- eval(formals(res$eval_fun)$interval) # what kinds of intervals are available
  
  res
}


