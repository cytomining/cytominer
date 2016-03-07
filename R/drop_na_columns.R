#' Select features without NA values
#'
#' @param population population
#' @param variables variables
#' @param ... Arguments to be passed to methods
#'
#' @return Excluded variables
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
drop_na_columns <- function(population, variables, ...) {
  population %>%
    dplyr::summarise_each_(dplyr::funs(count), vars = variables) %>%
    dplyr::collect() %>%
    tidyr::gather(feature, count) %>%
    dplyr::filter(count == 0) %>%
    magrittr::extract2("feature")
}
