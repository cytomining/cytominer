#' Measure variable importance
#'
#' @param population ...
#' @param variables ...
#' @param operation ...
#' @param ... arguments passed to variable importance operation
#'
#' @return variable importance measures
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
variable_importance <- function(population, variables, operation = "replicate_correlation", ...) {
  if (operation == "replicate_correlation") {
   importance <- replicate_correlation(population, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  importance
}
