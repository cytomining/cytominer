#' Select features without NA values
#'
#' @param population ...
#' @param variables ...
#'
#' @return Excluded variables
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
drop_na_columns <- function(population, variables) {
  population %>%
    dplyr::summarise_each_(dplyr::funs_("count"), vars = variables) %>%
    dplyr::collect() %>%
    tidyr::gather_("feature", "count", variables) %>%
    dplyr::filter_( ~ (count == 0) ) %>%
    magrittr::extract2("feature")
}
