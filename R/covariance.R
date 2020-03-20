#' Compute covariance matrix and vectorize.
#'
#' \code{covariance} computes the covariance matrix and vectorize it.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#'
#' @return data frame of 1 row comprising vectorized covariance matrix.
#'
#' @examples
#'
#' population <- tibble::tibble(
#'   x = rnorm(30),
#'   y = rnorm(30),
#'   z = rnorm(30)
#' )
#'
#' variables <- c("x", "y")
#'
#' covariance(population, variables)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
covariance <- function(population, variables) {
  covariance <-
    population %>%
    dplyr::select_at(variables) %>%
    stats::cov()

  variable_pairs <-
    outer(
      variables,
      variables,
      function(var1, var2) paste(var1, "__", var2, sep = "")
    )

  mask <- lower.tri(covariance, diag = T)

  covariance <- covariance[mask]

  names(covariance) <- variable_pairs[mask]

  covariance %>%
    t() %>%
    as.data.frame()
}
