#' Counts the frequency of NAs per variable.
#'
#' @param population Data frame with observation and grouping variables (metadata).   
#' @param variables Vector of column names defining the used features.
#'
#' @return Data frame with frequency of NAs per variable
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' 
#'@examples
#'
#'  population <- tibble::data_frame(
#'    Metadata_group = c("control", "control","control","control","experiment","experiment","experiment","experiment"),
#'    Metadata_batch = c("a","a","b","b","a","a","b","b"),
#'    AreaShape_Area = c(10,12,15,16,8,8,7,7),
#'    AreaShape_length = c(2,3,NA,NA,4,5,1,5)
#'  )
#' variables <- c('AreaShape_Area','AreaShape_length')
#' na_per_row = count_na_rows(population, variables)
#' 
#' @export
count_na_rows <- function(population, variables) {
  # nrows <-
  #   population %>%
  #   dplyr::tally() %>%
  #   dplyr::collect() %>%
  #   magrittr::extract2("n")
  #
  # nrows - (
  #   population %>%
  #   dplyr::summarise_each_(dplyr::funs_("count"), vars = variables) %>%
  #   dplyr::collect()
  # )

  population %>%
    dplyr::mutate_at(variables, dplyr::funs(is.na)) %>%
    dplyr::summarize_at(variables, dplyr::funs(sum)) %>%
    dplyr::collect() %>%
    data.frame()
}
