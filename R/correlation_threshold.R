#' Select features of a data.frame using correlation threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param cutoff cutoff
#' @param method method
#' @param verbose verbose
#'
#' @return data.frame after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
correlation_threshold <- function(population, variables, sample, cutoff = 0.90, method = 'pearson', verbose = FALSE) {

  features_exclude <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    findCorrelation(cutoff = cutoff, verbose = verbose)

  population %>%
    dplyr::select_(.dots = variables[-features_exclude])

}
