#' Transform observation variables.
#'
#' \code{transform} transforms observation variables based on the specified
#' transformation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for transform.
#'   This must be one of the strings \code{"generalized_log"} (default),
#'   \code{"spherize"}, or \code{"sparse_random_projection"}.
#' @param ... arguments passed to transformation operation.
#'
#' @return transformed data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_Well = c("A01", "A02", "B01", "B02"),
#'   Intensity_DNA = c(8, 20, 12, 32),
#'   Intensity_RNA = c(1, 12, -1, 4),
#'   Intensity_AGP = c(-2, 5, -5, -2),
#'   Intensity_Mito = c(-1, 15, 5, 22),
#'   Intensity_ER = c(-12, 15, -25, 24)
#' )
#' variables <- c("Intensity_DNA", "Intensity_RNA", "Intensity_AGP", "Intensity_ER")
#' transform(population, variables, operation = "generalized_log")
#' transform(population, variables, sample = population, operation = "husk", remove_outliers = FALSE)
#' transform(population, variables, sample = population, operation = "spherize")
#' transform(population, variables, n_components = 2, operation = "sparse_random_projection")
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
transform <- function(population, variables,
                      operation = "generalized_log", ...) {
  if (operation == "generalized_log") {
    generalized_log(population, variables, ...)
  } else if (operation == "spherize") {
    spherize(population, variables, ...)
  } else if (operation == "husk") {
    husk(population, variables, ...)
  } else if (operation == "sparse_random_projection") {
    sparse_random_projection(population, variables, ...)
  } else {
    error <- paste0("undefined operation '", operation, "'")

    stop(error)
  }
}
