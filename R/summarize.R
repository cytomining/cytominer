#' Summarize data
#'
#' @param population ...
#' @param variables ...
#' @param operation ...
#' @param ... arguments passed to summarization operation
#'
#' @return summarized data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
summarize <- function(population, variables, operation, ...) {
  if (operation == "trajectory") {
    summarized <- tracks(population, ...)

  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }
}