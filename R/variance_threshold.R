#' Select features using variance threshold
#'
#' @param variables Vector of column names defining the used features.
#' @param sample Subpopulation used to calculate the distribution of the given variables.
#'
#' @return Excluded variables
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

    which((ratio > 19 & (100 * lunique / apply(x, 2, length)) <= 10) | (lunique == 1) | apply(x, 2, function(data) all(is.na(data))))
  }

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    near_zero_variance()

  variables[excluded_indexes]
}
