#' Transform data
#'
#' @param population ...
#' @param variables ...
#' @param operation ...
#' @param ... arguments passed to transformation operation
#'
#' @return transformed data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
transform <- function(population, variables, operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }
}
