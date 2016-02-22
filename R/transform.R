#' Transform rows of a data.frame
#'
#' @param population population
#' @param sample sample
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after transformation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
transform <- function(population, sample = NULL, operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population)
  } else {
    stop("unknown operation")
  }
}
