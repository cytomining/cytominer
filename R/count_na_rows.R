#' Frequency of NAs per variable
#'
#' @param population ...
#' @param variables ...
#'
#' @return data.frame with frequency of NAs per variable
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
count_na_rows <- function(population, variables) {
  nrows <-
    population %>%
    dplyr::summarise(count = n()) %>%
    dplyr::collect() %>%
    magrittr::extract2("count")

  nrows - (
    population %>%
    dplyr::summarise_each_(dplyr::funs_("count"), vars = variables) %>%
    dplyr::collect()
  )
}
