#' Select observation variables
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for variable selection. This must be one of the strings "variance_threshold", "correlation_threshold", "drop_na_columns".
#' @param sample tbl containing sample that is used by some variable selection methods. `sample` has same structure as `population`.
#' @param ... arguments passed to selection operation.
#'
#' @return variable-selected data of the same class as `population`.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
select <- function(population, variables, sample = NULL,
                   operation = "variance_threshold", ...) {
  if (operation == "variance_threshold") {
    excluded <- variance_threshold(variables, sample, ...)
  } else if (operation == "correlation_threshold") {
    excluded <- correlation_threshold( variables, sample, ...)
  } else if (operation == "drop_na_columns") {
    excluded <- drop_na_columns(population, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  if (length(excluded) > 0) {
    futile.logger::flog.info("excluded:")

    for (e in excluded) {
      futile.logger::flog.info(paste("\t", e))
    }
  }

  population %>%
    dplyr::select_(.dots = setdiff(x = colnames(population), y = excluded))
}
