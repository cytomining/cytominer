#' Generalized log of rows of a data.frame
#'
#' @param population population
#' @param variables variables
#' @param c shift
#'
#' @return data.frame after transformation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
generalized_log <- function(population, variables, c = 1) {
  for (variable in variables) {
    population %<>%
      dplyr::mutate_(
        .dots = setNames(
          list(
            lazyeval::interp(
              ~ log((x + (x^2 + c^2)^0.5) / 2),
              x = as.name(variable),
              c = c
            )
          ),
          paste0(variable, '_')
        )
      )
  }

  population %>%
    dplyr::select_(
      .dots = paste0(variables, '_')
    )  %>%
    dplyr::rename_(
      .dots = setNames(paste0(variables, '_'), variables)
    )

}
