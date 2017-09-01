utils::globalVariables(c("count"))
#' Select features without NA values
#'
#' @param population Data frame with observation and grouping variables (metadata). 
#' @param variables Vector of column names defining the used variables.
#' @param cutoff Threshold for the frequency of na values between 0 and 1. All features with a NA frequency > cutoff are selected. Default value cutoff = 0.05.
#'
#' @return List with names of the excluded features.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' 
#' @examples
#'  population <- tibble::data_frame(
#'    Metadata_group = c("control", "control","control","control",
#'                       "experiment","experiment","experiment","experiment"),
#'    Metadata_batch = c("a","a","b","b","a","a","b","b"),
#'    AreaShape_Area = c(10,12,15,16,8,8,7,7),
#'    AreaShape_length = c(2,3,NA,NA,4,5,1,5)
#'  )
#' variables <- c('AreaShape_Area','AreaShape_length')
#' na_per_column = drop_na_columns(population, variables)
#' 
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
