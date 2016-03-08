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
aggregate <- function(population, variables, grouping_variables,
                      operation="mean", ...) {

  worker <- function(aggregating_function) {
    population %>%
      dplyr::group_by_(.dots = grouping_variables) %>%
      dplyr::summarise_each_(aggregating_function, vars = variables) %>%
      dplyr::ungroup()
  }

  if (operation == "mean") {
    worker(aggregating_function = dplyr::funs(mean))
  } else if (operation == "median") {
    worker(aggregating_function = dplyr::funs(median))
  } else {
    stop("unknown operation")
  }
}
