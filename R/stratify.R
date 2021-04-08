#' Stratify operations.
#'
#' \code{stratify} stratifies operations.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param strata optional character vector specifying grouping variables for
#'   stratification.
#' @param sample tbl with the same structure as \code{population}. This is
#'   typically used by operations to estimate parameters.
#' @param reducer operation that is to applied in a stratified manner.
#' @param ... arguments passed to operation.
#'
#' @return \code{population} with potentially extra columns.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 100, replace = TRUE),
#'   Metadata_type = sample(c("control", "trt"), 100, replace = TRUE),
#'   AreaShape_Area = c(rnorm(98), 20, 30),
#'   AreaShape_Eccentricity = rnorm(100)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_Eccentricity")
#' strata <- c("Metadata_group")
#' sample <- population %>% dplyr::filter(Metadata_type == "control")
#' population_marked <-
#'   cytominer::stratify(
#'     reducer = cytominer::mark_outlier_rows,
#'     method = "svd+iqr",
#'     population = population,
#'     variables = variables,
#'     sample = sample,
#'     strata = strata
#'   )
#' population_marked %>%
#'   dplyr::group_by(is_outlier) %>%
#'   dplyr::sample_n(3)
#' @export
stratify <- function(population,
                     sample,
                     reducer,
                     strata,
                     ...) {
  reduct <- function(partition) {
    sample_partition <-
      dplyr::inner_join(sample, partition, by = names(partition))

    population_partition <-
      dplyr::inner_join(population, partition, by = names(partition))

    reducer(
      population = population_partition,
      sample = sample_partition,
      ...
    )
  }

  output <-
    sample %>%
    dplyr::select(all_of(strata)) %>%
    dplyr::group_by(across(all_of(strata))) %>%
    dplyr::summarise(reduct(dplyr::cur_group())) %>%
    dplyr::ungroup()

  output
}
