#' Select observation variables.
#'
#' \code{variable_select} selects observation variables based on the specified variable selection method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for variable selection. This must be one of the strings \code{"variance_threshold"}, \code{"correlation_threshold"}, \code{"drop_na_columns"}.
#' @param sample tbl containing sample that is used by some variable selection methods. \code{sample} has same structure as \code{population}.
#' @param ... arguments passed to selection operation.
#'
#' @return variable-selected data of the same class as \code{population}.
#'
#' @examples
#'
#' # In this example, we use `correlation_threshold` as the operation for
#' # variable selection.
#'
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   x = rnorm(100),
#'   y = rnorm(100) / 1000
#' )
#'
#' population %<>% dplyr::mutate(z = x + rnorm(100) / 10)
#'
#' sample <- population %>% dplyr::slice(1:30)
#'
#' variables <- c("x", "y", "z")
#'
#' operation <- "correlation_threshold"
#'
#' cor(sample)
#'
#' # `x` and `z` are highly correlated; one of them will be removed
#'
#' head(population)
#'
#' futile.logger::flog.threshold(futile.logger::ERROR)
#'
#' variable_select(population, variables, sample, operation) %>% head()
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
variable_select <- function(population, variables, sample = NULL,
                            operation = "variance_threshold", ...) {
  if (operation == "variance_threshold") {
    excluded <- variance_threshold(variables, sample, ...)
  } else if (operation == "correlation_threshold") {
    excluded <- correlation_threshold(variables, sample, ...)
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
    dplyr::select(setdiff(x = colnames(population), y = excluded))
}
