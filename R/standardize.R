#' Standardize rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
standardize <- function(population, variables, sample) {
  mu <- sample %>% dplyr::summarise_each_(dplyr::funs(mean), vars = variables) %>% dplyr::collect()

  sigma <- sample %>% dplyr::summarise_each_(dplyr::funs(sd), vars = variables) %>% dplyr::collect()

  population %>% scale_dplyr(center = mu, scale = sigma, vars = variables)
}
