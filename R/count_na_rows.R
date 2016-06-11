#' Frequency of NAs per variable
#'
#' @param population ...
#' @param variables ...
#'
#' @return data.frame with frequency of NAs per variable
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
count_na_rows <- function(population, variables) {
  count_na <- function(x) sum(is.na(x))
  
  population %>%
    dplyr::summarise_each_(dplyr::funs(count_na_rows), vars = variables) %>%
    dplyr::collect()
}
