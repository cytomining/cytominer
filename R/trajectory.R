#' Compute trajectory statistics
#'
#' @param population ...
#'
#' @return trajectory statistics
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
trajectory <- function(population, ...) {
  # process `population`, which is the data you get from CellProfiler
  
  # a dummy result
  tibble::data_frame(speed = 1)
}