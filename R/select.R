#' Select columns
#'
#' @param population population
#' @param variables variables
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return object after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
select <- function(population, variables, operation = "variance_threshold", ...) {
  if (operation == "variance_threshold") {
    excluded <- variance_threshold(population, variables, ...)
  } else if (operation == "correlation_threshold") {
    excluded <- correlation_threshold(population, variables, ...)
  } else if (operation == "drop_na_columns") {
    excluded <- cytominr::drop_na_columns(population, variables, ...)
  } else {
    stop("unknown operation")
  }

  variables <-
    setdiff(x = colnames(population), y = excluded)

  population %>%
    dplyr::select_(.dots = variables)
}
