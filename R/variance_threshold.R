#' Select features of a data.frame using variance threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
variance_threshold <- function(population, variables, sample, ...) {

  features_exclude <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    nearZeroVar()

  population %>%
    dplyr::select_(.dots = variables[-features_exclude])

}
