#' Aggregate rows
#'
#' @param population population
#' @param variables variables
#' @param grouping_variables grouping_variables
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after aggregation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
aggregate <- function(population, variables, grouping_variables, operation="mean", ...) {
  if (operation == "mean") {
    population %>%
      dplyr::group_by_(.dots = grouping_variables) %>%
      dplyr::summarise_each_(dplyr::funs(mean), vars = variables) %>%
      dplyr::ungroup()
  } else if (operation == "median") {
    population %>%
      dplyr::group_by_(.dots = grouping_variables) %>%
      dplyr::summarise_each_(dplyr::funs(median), vars = variables) %>%
      dplyr::ungroup()
  } else {
    stop("unknown operation")
  }
}
