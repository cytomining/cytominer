#' Transform observation variables.
#'
#' \code{transform} transforms observation variables based on the specified transformation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for transform. Currently, only \code{"generalized_log"} (default) is implemented.
#' @param ... arguments passed to transformation operation.
#'
#' @return transformed data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::data_frame(
#'    Metadata_Well = c("A01", "A02", "B01", "B02"),
#'    Intensity_DNA = c(8, 20, 12, 32)
#'  )
#' variables <- c("Intensity_DNA")
#' transform(population, variables, operation = "generalized_log")
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
transform <- function(population, variables,
                      operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population, variables, ...)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }
}
