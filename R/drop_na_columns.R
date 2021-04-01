utils::globalVariables(c("na_count", "na_percent"))
#' Remove variables with NA values.
#'
#' \code{drop_na_columns} returns list of variables which have greater than a
#' specified threshold number of \code{NA}s.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param cutoff threshold between [0,1]. Variables with an \code{NA}
#'   frequency > \code{cutoff} are returned.
#'
#' @return character vector specifying observation variables to be excluded.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7),
#'   AreaShape_Length = c(2, 3, NA, NA, 4, 5, 1, 5)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_Length")
#' drop_na_columns(population, variables)
#' @export
drop_na_columns <- function(population, variables, cutoff = 0.05) {
  nrows <-
    population %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    magrittr::extract2("n")

  # TODO: Migrate to `dplyr::across` once this issue is fixed
  # https://github.com/tidyverse/dbplyr/issues/480#issuecomment-811814636
  population %>%
    dplyr::mutate_at(variables, is.na) %>%
    dplyr::summarize_at(variables, ~ sum(., na.rm = T)) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(everything(),
                        names_to = "variable",
                        values_to = "na_count") %>%
    dplyr::mutate(na_percent = na_count / nrows) %>%
    dplyr::filter(na_percent > cutoff) %>%
    magrittr::extract2("variable")
}
