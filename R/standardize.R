#' Standardize rows of a data.frame
#'
#' @param population population
#' @param sample sample
#'
#' @return data.frame after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
standardize <- function(population, sample) {
  mu <- sample %>% dplyr::summarise_each(dplyr::funs(mean))

  sigma <- sample %>% dplyr::summarise_each(dplyr::funs(sd))

  population %>% dplyr::mutate_each(dplyr::funs(. - mu$.)) %>%
    dplyr::mutate_each(dplyr::funs(. / sigma$.))
}

