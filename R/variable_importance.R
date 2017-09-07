#' Measure variable importance.
#'
#' \code{variable_importance} measures importance of variables based on specified methods.
#' 
#' @param sample tbl containing sample used to estimate parameters.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for computing variable importance. Currently, only \code{"replicate_correlation"} (default) is implemented.
#' @param ... arguments passed to variable importance operation.
#'
#' @return data frame containing variable importance measures.
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
