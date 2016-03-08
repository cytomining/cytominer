#' Select columns
#'
#' @param population ...
#' @param variables ...
#' @param operation ...
#' @param ... arguments passed to selection operation
#'
#' @return feature selected data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
select <- function(population, variables,
                   operation = "variance_threshold", ...) {
  if (operation == "variance_threshold") {
    excluded <- variance_threshold(population, variables, ...)
  } else if (operation == "correlation_threshold") {
    excluded <- correlation_threshold(population, variables, ...)
  } else if (operation == "drop_na_columns") {
    excluded <- drop_na_columns(population, variables, ...)
  } else {
    stop("unknown operation")
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
