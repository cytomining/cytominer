#' Remove redundant variables.
#'
#' \code{correlation_threshold} returns list of variables such that no two variables have a correlation greater than a specified threshold.
#'
#' \code{correlation_threshold} is a wrapper for \code{caret::findCorrelation}.
#'
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample used to estimate parameters.
#' @param cutoff threshold between [0,1] that defines the minimum correlation of a selected feature.
#' @param method optional character string specifying method for calculating correlation. This must be one of the strings \code{"pearson"} (default), \code{"kendall"}, \code{"spearman"}.
#'
#' @return character vector specifying observation variables to be excluded.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @examples
#'
#' suppressMessages(suppressWarnings(library(magrittr)))
#' sample <- tibble::tibble(
#'   x = rnorm(30),
#'   y = rnorm(30) / 1000
#' )
#'
#' sample %<>% dplyr::mutate(z = x + rnorm(30) / 10)
#' variables <- c("x", "y", "z")
#'
#' head(sample)
#' cor(sample)
#'
#' # `x` and `z` are highly correlated; one of them will be removed
#'
#' correlation_threshold(variables, sample)
#' @export
correlation_threshold <- function(variables, sample, cutoff = 0.90,
                                  method = "pearson") {
  .variables <- rlang::syms(variables)

  excluded_indexes <-
    sample %>%
    dplyr::select(!!!.variables) %>%
    cor(method = method) %>%
    caret::findCorrelation(cutoff = cutoff)

  variables[excluded_indexes]
}
