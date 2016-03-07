#' Select features using variance threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param ... Arguments to be passed to methods
#'
#' @return Excluded variables
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
variance_threshold <- function(population, variables, sample, ...) {

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    nearZeroVar()

  variables[excluded_indexes]

}
