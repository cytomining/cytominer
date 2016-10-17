utils::globalVariables(c("count"))
#' Select features without NA values
#'
#' @param population ...
#' @param variables ...
#' @param cutoff ...
#'
#' @return Excluded variables
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
drop_na_columns <- function(population, variables, cutoff = 0.05) {
  # population %>%
  #   dplyr::summarise_each_(dplyr::funs_("count"), vars = variables) %>%
  #   dplyr::collect() %>%
  #   tidyr::gather_("feature", "count", variables) %>%
  #   dplyr::filter_( ~ (count == 0) ) %>%
  #   magrittr::extract2("feature")

  nrows <-
    population %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    magrittr::extract2("n")

  population %>%
    dplyr::mutate_at(variables, dplyr::funs(is.na)) %>%
    dplyr::summarize_at(variables, dplyr::funs(sum)) %>%
    dplyr::collect() %>%
    tidyr::gather_("feature", "count", variables) %>%
    dplyr::mutate(percent = count / nrows) %>%
    dplyr::filter_( ~ (percent > cutoff) ) %>%
    magrittr::extract2("feature")
}
