#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for aggregation.
#' @param operation optional character string specifying method for aggregation. This must be one of the strings \code{"mean"}, \code{"median"}, \code{"mean+sd"}.
#' @param ... optional arguments passed to aggregation operation
#'
#' @return aggregated data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::data_frame(
#'    Metadata_group = c("control", "control", "control", "control",
#'                       "experiment", "experiment", "experiment", "experiment"),
#'    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
#'  )
#' variables <- c("AreaShape_Area")
#' strata <- c("Metadata_group", "Metadata_batch")
#' aggregate(population, variables, strata, operation = "mean")
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(population, variables, strata, operation="mean", ...) {

  if (operation == "mean") {
    aggregating_function <- dplyr::funs(mean)
  } else if (operation == "median") {
    aggregating_function <- dplyr::funs(median)
  } else if (operation == "mean+sd") {
    aggregating_function <- c(dplyr::funs(mean), dplyr::funs(sd))
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  population %>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::summarise_at(.funs = aggregating_function, .vars = variables) %>%
    dplyr::ungroup()
}
