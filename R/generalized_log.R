#' Generalized log transform data
#'
#' @param population ...
#' @param variables ...
#' @param c ...
#'
#' @return generalized log transformed data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
generalized_log <- function(population, variables, c = 1) {
  column_names <- colnames(population)

  for (variable in variables) {

    object <- list(lazyeval::interp(~log( (x + (x ^ 2 + c ^ 2) ^ 0.5 ) / 2), x = as.name(variable), c = c))

    name <- paste0(variable, "_")

    population %<>%
      dplyr::mutate_(.dots = setNames(object, name))
  }

  population %>%
    dplyr::select_(~-one_of(variables))  %>%
    dplyr::rename_(.dots = setNames(paste0(variables, "_"), variables)) %>%
    dplyr::select_(.dots = column_names)
}
