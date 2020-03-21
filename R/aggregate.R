utils::globalVariables("data")
#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for aggregation.
#' @param operation optional character string specifying method for aggregation,
#'   e.g. \code{"mean"}, \code{"median"}, \code{"mean+sd"}. A sequence can
#'   comprise only of univariate functions.
#' @param univariate boolean specifying whether the aggregation function is
#'    univariate or multivariate.
#' @param ... optional arguments passed to aggregation operation
#'
#' @return aggregated data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment",
#'     "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
#' )
#' variables <- c("AreaShape_Area")
#' strata <- c("Metadata_group", "Metadata_batch")
#' aggregate(population, variables, strata, operation = "mean")
#' @importFrom utils find
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(population, variables, strata, operation = "mean",
                      univariate = TRUE,
                      ...) {
  strata <- rlang::syms(strata)

  # If the aggregation function is a multivariate function, dplyr::summarize
  # won't do the job because that operates on variable at a time.
  if (!univariate) {
    return(
      population %>%
        dplyr::collect() %>%
        dplyr::group_by(!!!strata) %>%
        tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(
          data,
          rlang::as_function(operation),
          variables
        )) %>%
        tidyr::unnest(cols = data) %>%
        dplyr::ungroup()
    )
  }

  # Check whether `operation` is a function, or a sequence of functions
  # separated by `+`
  # For simplicity, a sequence can comprise only of univariate functions
  if (stringr::str_split(operation, "\\+")[[1]] %>%
    purrr::map_lgl(function(f) {
      length(utils::find(f, mode = "function")) == 0
    }) %>%
    any()
  ) {
    error <- paste0("undefined operation '", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  # construct aggregation_function

  # In dplyr::summarize, function names will be included only if `.funs`` has
  # names or multiple inputs

  # median has special handling because of this issue
  # https://github.com/tidyverse/dbplyr/issues/357#issuecomment-548850817
  if (length(stringr::str_split(operation, "\\+")[[1]]) == 1) {
    if (!is.data.frame(population)) {
      operation <- ifelse(operation == "median",
        "~MEDIAN(.)",
        sprintf("~%s(., na.rm = TRUE)", operation)
      )

      operation <- stats::as.formula(operation)
    }

    aggregating_function <- operation
  } else {
    aggregating_function <-
      stringr::str_split(operation, "\\+")[[1]] %>%
      as.vector() %>%
      unname()

    if (!is.data.frame(population)) {
      aggregating_function <-
        aggregating_function %>%
        purrr::map_chr(~ ifelse(.x == "median",
          "~MEDIAN(.)",
          sprintf("~%s(., na.rm = TRUE)", .x)
        )) %>%
        purrr::map(stats::as.formula)
    }
  }

  population %>%
    dplyr::group_by(!!!strata) %>%
    dplyr::summarise_at(.funs = aggregating_function, .vars = variables) %>%
    dplyr::ungroup()
}
