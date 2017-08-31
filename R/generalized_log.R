#' Generalized log transform data
#' 
#' Each variable is log transformed using x = log( (x + sqrt(x ^ 2 + offset ^ 2 )) / 2 ).
#'
#' @param population Data frame with observation and grouping variables (metadata). 
#' @param variables Vector of column names defining the used features. 
#' @param offset Offset parameter for the log transformation. Default offset = 1.
#'
#' @return Data frame with generalized log transformed values.
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

