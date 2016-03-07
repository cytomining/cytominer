#' Select features using correlation threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param cutoff cutoff
#' @param method method
#' @param verbose verbose
#'
#' @return Excluded variables
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
correlation_threshold <- function(population, variables, sample, cutoff = 0.90, method = "pearson", verbose = FALSE) {

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    findCorrelation(cutoff = cutoff, verbose = verbose)

  variables[excluded_indexes]
}
