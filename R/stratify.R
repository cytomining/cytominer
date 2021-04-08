#' Stratify operations.
#'
#' \code{stratify} stratifies operations.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata optional character vector specifying grouping variables for
#'   stratification. If \code{NULL}, no stratification is performed.
#' @param sample tbl containing sample that is used by operations to estimate
#'   parameters. \code{sample} has same structure as \code{population}.
#' @param operation operation that is to applied in a stratified manner.
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
#'     operation = cytominer::mark_outlier_rows,
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
                     variables,
                     sample,
                     operation,
                     strata,
                     ...) {
  groups <-
    sample %>%
    dplyr::select(all_of(strata)) %>%
    dplyr::distinct()

  output <-
    Reduce(
      dplyr::union_all,
      Map(
        f = function(group) {
          sample_group <-
            dplyr::inner_join(sample, group, by = names(group))

          population_group <-
            dplyr::inner_join(population, group, by = names(group))

          operation(
            population = population_group,
            variables = variables,
            sample = sample_group,
            ...
          )
        },
        split(x = groups, f = seq(nrow(groups)))
      )
    )

  output
}
