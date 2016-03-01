#' Generalized log of rows of a data.frame
#'
#' @param population population
#' @param c shift
#'
#' @return data.frame after transformation
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
generalized_log <- function(population, c=1) {
  glog <- function(x) log((x + sqrt(x^2 + c^2)) / 2 )
  
  population %>% dplyr::mutate_each(dplyr::funs(glog))
}
