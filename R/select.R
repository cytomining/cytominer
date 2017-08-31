#' Select columns
#'
#' @param population Data frame with observation and grouping variables (metadata). 
#' @param variables  Vector of column names defining the used features. 
#' @param operation One of 'variance_threshold', 'correlation_threshold', 'drop_na_columns'. 
#' @param sample (Sub)population used with operation 'correlation_threshold' and 'variance_threshold'. Default sample = NULL.
#' @param ... arguments passed to selection operation
#'
#' @return feature selected data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
select <- function(population, variables,
                   operation = "variance_threshold", ...) {
  if (operation == "variance_threshold") {
    excluded <- variance_threshold(population, variables, ...)
  } else if (operation == "correlation_threshold") {
    excluded <- correlation_threshold(population, variables, ...)
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
