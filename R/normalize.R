#' Normalize rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param grouping_variables grouping_variables
#' @param sample sample
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize <- function(population, variables, grouping_variables, sample, operation = "standardize", ...) {
  if (operation == "linearize") {
    linearize(population, variables, grouping_variables, sample)
  } else if (operation == "robustize") {
    robustize(population, variables, grouping_variables, sample)
  } else if (operation == "standardize") {
    standardize(population, variables, grouping_variables, sample)
  } else {
    stop("unknown operation")
  }
}
