#' Measure variable importance.
#'
#' \code{variable_importance} measures importance of variables based on specified methods.
#' 
#' @param sample tbl containing sample used to estimate parameters.
#' @param variables character vector specifying observation variables.
#' @param operation optional character string specifying method for computing variable importance. Currently, only \code{"replicate_correlation"} (default) is implemented.
#' @param ... arguments passed to variable importance operation.
#'
#' @return data frame containing variable importance measures.
#'
#' @examples
#' set.seed(123)
#' x1 <- rnorm(10)
#' x2 <- x1 + rnorm(10) / 100
#' y1 <- rnorm(10)
#' y2 <- y1 + rnorm(10) / 10
#' z1 <- rnorm(10)
#' z2 <- z1 + rnorm(10) / 1
#' 
#' batch <- rep(rep(1:2, each=5), 2)
#' 
#' treatment <- rep(1:10, 2)
#' 
#' replicate_id <- rep(1:2, each=10)
#' 
#' sample <- 
#'   tibble::data_frame(x = c(x1, x2), y = c(y1, y2), z = c(z1, z2),
#'                      Metadata_treatment = treatment, 
#'                      Metadata_replicate_id = replicate_id, 
#'                      Metadata_batch = batch)
#' 
#' head(sample)
#' 
#' # `replicate_correlation`` returns the median, min, and max 
#' # replicate correlation (across batches) per variable
#' variable_importance(sample = sample,
#'                     variables = c("x", "y", "z"),
#'                     operation = "replicate_correlation",
#'                     strata = c("Metadata_treatment"),
#'                     replicates = 2,
#'                     split_by = "Metadata_batch",
#'                     replicate_by = "Metadata_replicate_id",
#'                     cores = 1) 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
variable_importance <- function(sample, variables,
                                operation = "replicate_correlation", ...) {
  if (operation == "replicate_correlation") {
   importance <- replicate_correlation(sample, variables, ...)

  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)

  }

  importance
}
