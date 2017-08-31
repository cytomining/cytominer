#' Transform data
#'
#' Method to transform the valus in the data frame. This is often helpfull 
#' if the distribution of features does not follow a gaussian distribution. 
#'
#' @param population Data frame with observation and grouping variables (metadata).
#' @param variables Vector of column names defining the used features.
#' @param operation One of 'generalized log'. Default  operation = "generalized_log".
#' @param ... arguments passed to transformation operation
#'
#' @return transformed data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
transform <- function(population, variables, operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }
}
