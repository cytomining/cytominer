#' Select features using variance threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param ... Arguments to be passed to methods
#'
#' @return object after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
variance_threshold <- function(population, variables, sample, ...) {

  features_exclude <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    nearZeroVar()

  if(length(features_exclude) > 0)
    variables <- variables[-features_exclude]

  population %>%
    dplyr::select_(.dots = variables)

}
