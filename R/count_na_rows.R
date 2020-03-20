#' Count the number of \code{NA}s per variable.
#'
#' \code{count_na_rows} counts the number of \code{NA}s per variable.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#'
#' @return data frame with frequency of \code{NA}s per variable.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#'
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7),
#'   AreaShape_length = c(2, 3, NA, NA, 4, 5, 1, 5)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_length")
#' count_na_rows(population, variables)
#' @export
count_na_rows <- function(population, variables) {
  population %>%
    dplyr::mutate_at(variables, is.na) %>%
    dplyr::summarize_at(variables, ~ sum(., na.rm = T)) %>%
    dplyr::collect() %>%
    data.frame()
}
