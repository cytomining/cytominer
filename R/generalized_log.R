#' Generalized log of rows
#'
#' @param population population
#' @param variables variables
#' @param c shift
#'
#' @return object after transformation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
generalized_log <- function(population, variables, c = 1) {
  for (variable in variables) {
    population %<>%
      dplyr::mutate_(
        .dots = setNames(
          list(
            lazyeval::interp(
              ~ log( (x + ( x ^ 2 + c ^ 2) ^ 0.5 ) / 2 ),
              x = as.name(variable),
              c = c
            )
          ),
          paste0(variable, "_")
        )
      )
  }

  population %>%
    dplyr::select(-one_of(variables))  %>%
    dplyr::rename_(.dots = setNames(paste0(variables, "_"), variables))

}
