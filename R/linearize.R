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
linearize <- function(population, variables, sample, lower_quantile = 0.25, upper_quantile = 0.75) {

  # TODO: Currently, (0.25, 0.75) are imposed because lower_quartile and
  # upper_quartile are already implemented in sqlite, whereas quantile is not
  stopifnot(lower_quantile == 0.25 & upper_quantile == 0.75)

  if ("data.frame" %in% class(population)) {
    lower_quartile <- function(x) {
      quantile(x, probs = c(lower_quantile))[[1]]
    }

    upper_quartile <- function(x) {
      quantile(x, probs = c(upper_quantile))[[1]]
    }
  }

  m <- sample %>% dplyr::summarise_each_(dplyr::funs(lower_quartile), vars = variables) %>% dplyr::collect()

  n <- sample %>% dplyr::summarise_each_(dplyr::funs(upper_quartile), vars = variables) %>% dplyr::collect()

  quantile_range <- n - m

  population %>% scale_dplyr(center = m, scale = quantile_range, vars = variables)
}
