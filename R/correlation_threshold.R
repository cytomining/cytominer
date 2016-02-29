#' Select features of a data.frame using correlation threshold
#'
#' @param population population
#' @param sample sample
#' @param cutoff cutoff
#' @param method method
#' @param verbose verbose
#'
#' @return data.frame after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
correlation_threshold <- function(population, sample, cutoff = 0.90, method = 'pearson', verbose = FALSE) {
  population[-(
      sample %>%
      cor(
        method = method
      ) %>%
      findCorrelation(
        cutoff = cutoff,
        verbose = verbose
      )
  )]
}
