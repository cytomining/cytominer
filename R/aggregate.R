#' Aggregate data
#'
#' @param population ...
#' @param variables ...
#' @param strata ...
#' @param operation ...
#' @param ... arguments passed to aggregation operation
#'
#' @return aggregated data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
aggregate <- function(population, variables, strata, operation="mean", ...) {

  if (operation == "mean") {
    aggregating_function <- dplyr::funs(mean)
  } else if (operation == "median") {
    aggregating_function <- dplyr::funs(median)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  population %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::summarise_each_(aggregating_function, vars = variables) %>%
    dplyr::ungroup()
}
