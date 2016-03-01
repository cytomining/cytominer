#' Robustize rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param sample sample
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
robustize <- function(population, variables, sample) {
  mu <- sample %>% dplyr::summarise_each(dplyr::funs(median))

  sigma <- sample %>% dplyr::summarise_each(dplyr::funs(mad))

  population %>% dplyr::mutate_each(dplyr::funs(. - mu$.)) %>%
    dplyr::mutate_each(dplyr::funs(. / sigma$.))
}

