#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for aggregation.
#' @param operation optional character string specifying method for aggregation, e.g. \code{"mean"}, \code{"median"}, \code{"mean+sd"}.
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
#' @importFrom utils find
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(population, variables, strata, operation="mean", ...) {

  # check whether `operation` is a function, or a sequence of functions
  # separated by `+`
  if (stringr::str_split(operation, "\\+")[[1]] %>%
      purrr::map_lgl(function(f)
        length(utils::find(f, mode = "function")) == 0) %>%
      any()
      ) {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)

  }

  # construct aggregation_function
  aggregating_function <-
    stringr::str_split(operation, "\\+")[[1]] %>%
    sapply(function(f) dplyr::funs(!!f)) %>%
    as.vector() %>%
    unname()

  population %<>%
    dplyr::group_by_(.dots = strata) %>%
    dplyr::summarise_at(.funs = aggregating_function, .vars = variables) %>%
    dplyr::ungroup()

  # if only one operation, dplyr::summarize does not append function name.
  # append explicitly
  if (length(stringr::str_split(operation, "\\+")[[1]]) == 1) {

    append_operation_tag <- function(s) stringr::str_c(s, operation, sep = "_")

    # TODO: remove dplyr::collect once
    # https://github.com/tidyverse/dplyr/issues/3132
    # is fixed
    population %<>%
      dplyr::collect() %>%
      dplyr::rename_at(variables, append_operation_tag)

  }

  population
}
