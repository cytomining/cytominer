#' Generalized log transform data.
#'
#' \code{generalized_log} transforms specified observation variables using \eqn{x = log( (x + sqrt(x ^ 2 + offset ^ 2 )) / 2 )}.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param offset offset parameter for the transformation.
#'
#' @return transformed data of the same class as \code{population}.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @export
generalized_log <- function(population, variables, offset = 1) {
  offset <- rlang::enquo(offset)

  for (variable in variables) {
    x <- rlang::sym(variable)

    population %<>%
      dplyr::mutate(!!x := log( ((!!x) + ((!!x) ^ 2 + (!!offset) ^ 2) ^ 0.5 ) / 2))
  }

  population
}

