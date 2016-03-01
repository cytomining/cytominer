#' Normalize rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize <- function(population, variables, sample, operation = "standardize", ...) {
  if (operation == "linearize") {
    linearize(population, variables, sample)
  } else if (operation == "robustize") {
    robustize(population, variables, sample)
  } else if (operation == "standardize") {
    standardize(population, variables, sample)
  } else {
    stop("unknown operation")
  }
}
