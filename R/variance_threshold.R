#' Remove variables with near-zero variance.
#'
#' \code{variance_threshold} returns list of variables that have near-zero variance.
#' 
#' \code{variance_threshold} is a reimplementation of \code{caret::nearZeroVar}, using 
#' the default values for \code{freqCut} and \code{uniqueCut}.
#' 
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample used to estimate parameters.
#'
#' @return character vector specifying observation variables to be excluded.
#'
#' @examples
#' sample <- tibble::data_frame(
#'    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7, 13, 18),
#'    AreaShape_Euler = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#'  )
#' variables <- c("AreaShape_Area", "AreaShape_Euler")
#' variance_threshold(variables, sample)
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
variance_threshold <- function(variables, sample) {
  near_zero_variance <- function (x) {
    if (is.null(dim(x))) x <- matrix(x, ncol = 1)

    ratio <- apply(x, 2, function(data) {
      t <- table(data[!is.na(data)])

      if (length(t) <= 1) return(0)

      return(max(t, na.rm = TRUE) / max(t[-which.max(t)], na.rm = TRUE))
    })

    lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))

    which( (ratio > 19 & (100 * lunique / apply(x, 2, length)) <= 10) |
            (lunique == 1) | apply(x, 2, function(data) all(is.na(data))))
  }

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    near_zero_variance()

  variables[excluded_indexes]
}
