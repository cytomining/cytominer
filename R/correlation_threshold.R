#' Select features using correlation threshold
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param cutoff cutoff
#' @param method method
#' @param verbose verbose
#'
#' @return object after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
correlation_threshold <- function(population, variables, sample, cutoff = 0.90, method = 'pearson', verbose = FALSE) {

  features_exclude <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    findCorrelation(cutoff = cutoff, verbose = verbose)

  if(length(features_exclude) > 0)
    variables <- variables[-features_exclude]

  population %>%
    dplyr::select(one_of(variables))

}
