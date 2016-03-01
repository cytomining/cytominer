#' Select features of a data.frame using variance threshold
#'
#' @param population population
#' @param variables variables
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
variance_threshold <- function(population, variables, ...) {
  population[-nearZeroVar(population)]
}
