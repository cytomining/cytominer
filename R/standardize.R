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
  μ <- sample %>% dplyr::summarise_each(dplyr::funs(mean)) %>% dplyr::collect()

  σ <- sample %>% dplyr::summarise_each(dplyr::funs(sd)) %>% dplyr::collect()

  population %>% scale_dplyr(center = μ, scale = σ, vars = names(μ))
}
