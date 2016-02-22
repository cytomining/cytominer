#' Normalize rows of a data.frame
#'
#' @param population population
#' @param sample sample
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
normalize <- function(population, sample, operation = "standardize", ...) {
  if (operation == "linear") {

  } else if (operation == "robust") {

  } else if (operation == "standardize") {
    standardize(population, sample)
  } else {

  }
}
