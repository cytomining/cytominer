#' Robustize rows
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#'
#' @return object after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
robustize <- function(population, variables, sample) {
  mu <- sample %>% dplyr::summarise_each_(dplyr::funs(median), vars = variables) %>% dplyr::collect()

  sigma <- sample %>% dplyr::summarise_each_(dplyr::funs(mad), vars = variables) %>% dplyr::collect()

  population %>% scale_dplyr(center = mu, scale = sigma, vars = variables)
}
