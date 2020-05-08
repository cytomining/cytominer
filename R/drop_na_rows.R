utils::globalVariables(c("key", "value", "rowname_temp", "rowid", "coalesce"))
#' Drop rows that are \code{NA} in all specified variables.
#'
#' \code{drop_na_rows} drops rows that are \code{NA} in all specified variables.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#'
#' @return \code{population} without rows that have \code{NA} in all specified variables.
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, NA, 16, 8, 8, 7, 7),
#'   AreaShape_Length = c(2, 3, NA, NA, 4, 5, 1, 5)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_Length")
#' drop_na_rows(population, variables)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
drop_na_rows <- function(population, variables) {
  if (is.data.frame(population)) {
    population %>%
      tibble::rowid_to_column() %>%
      tidyr::pivot_longer(variables) %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
      dplyr::select(-rowid) %>%
      dplyr::select(names(population))
  } else {

    # Coalesce() must have at least 2 arguments.
    if (length(variables) == 1) {
      variables <- c(variables, variables)
    }

    population %>%
      dplyr::filter(!is.null(coalesce(!!!rlang::syms(variables))))
  }
}
