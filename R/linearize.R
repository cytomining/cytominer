#' Linearize rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param lower_quantile lower_quantile
#' @param upper_quantile upper_quantile
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
linearize <- function(population, variables, sample, lower_quantile = 0.01, upper_quantile = 0.99) {
  m <-
    sample %>%
    dplyr::summarise_each(dplyr::funs(quantile(., c(lower_quantile))[[1]]))

  n <-
    sample %>%
    dplyr::summarise_each(dplyr::funs(quantile(., c(upper_quantile))[[1]]))

  quantile_range <-
    n - m

  population %>%
    dplyr::mutate_each(dplyr::funs(. - m$.)) %>%
    dplyr::mutate_each(dplyr::funs(. / quantile_range$.))
}
