#' Standardize rows
#'
#' @param population population
#' @param variables variables
#' @param grouping_variables grouping_variables
#' @param sample sample
#'
#' @return object after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
standardize <- function(population, variables, grouping_variables, sample) {
  mu <- sample %>% dplyr::summarise_each_(dplyr::funs(mean), vars = variables) %>% dplyr::collect()

  sigma <- sample %>% dplyr::summarise_each_(dplyr::funs(sd), vars = variables) %>% dplyr::collect()

  population %>% scale_dplyr(center = mu, scale = sigma, vars = variables)
}
