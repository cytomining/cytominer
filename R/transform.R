#' Transform rows
#'
#' @param population population
#' @param variables variables
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return object after transformation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
transform <- function(population, variables, operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population, variables, ...)
  } else {
    stop("unknown operation")
  }
}
