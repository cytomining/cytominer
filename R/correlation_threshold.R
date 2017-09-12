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
#' @examples
#' sample <- tibble::data_frame(
#'    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7, 13, 18),
#'    AreaShape_Euler = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#'  )
#' variables <- c("AreaShape_Area", "AreaShape_Euler")
#' correlation_threshold(variables, sample)
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
correlation_threshold <- function(variables, sample, cutoff = 0.90,
                                  method = "pearson") {

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    caret::findCorrelation(cutoff = cutoff)

  variables[excluded_indexes]
}
