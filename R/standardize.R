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
  μ <- sample %>% dplyr::summarise_each_(dplyr::funs(mean), vars = variables) %>% dplyr::collect()

  σ <- sample %>% dplyr::summarise_each_(dplyr::funs(sd), vars = variables) %>% dplyr::collect()

  population %>% scale_dplyr(center = μ, scale = σ, vars = variables)
}
