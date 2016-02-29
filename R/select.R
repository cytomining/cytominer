#' Select columns of a data.frame
#'
#' @param population population
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return data.frame after feature selection
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
select <- function(population, operation, ...) {
  if (operation == "variance_threshold") {
    variance_threshold(population)
  } else if (operation == "correlation_threshold") {
    correlation_threshold(population)
  } else {
    stop("unknown operation")
  }
}
