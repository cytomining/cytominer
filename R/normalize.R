#' Normalize rows of a data.frame
#'
#' @param population population
#' @param sample sample
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize <- function(population, sample, operation = "standardize", ...) {
  if (operation == "linearize") {
    linearize(population, sample)
  } else if (operation == "robustize") {
    robustize(population, sample)
  } else if (operation == "standardize") {
    standardize(population, sample)
  } else {
    stop("unknown operation")
  }
}
