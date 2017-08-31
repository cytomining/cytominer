#' Measure variable importance
#'
#'
#' @param sample Poplation used to estimate the importance of each variable. 
#' @param variables Vector of column names defining the used features. 
#' @param operation The operation "replicate_correlation" can be chosen. Default operation = "replicate_correlation". 
#' @param ... arguments passed to variable importance operation
#'
#' @return variable importance measures
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
variable_importance <- function(sample, variables, operation = "replicate_correlation", ...) {
  if (operation == "replicate_correlation") {
   importance <- replicate_correlation(sample, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  importance
}
