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

