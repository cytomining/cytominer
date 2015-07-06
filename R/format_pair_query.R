#' Format query contained in data frame
#'
#' @param cmp list containing two data.frames corresponding to the query pair
#' @param dict_column_names column names of the data.frame dictionary
#' that will be queried
#'
#' @return Formatted query
#'

format_pair_query <- function(cmp, dict_column_names) {
  v1_name <- cmp[[1]]
  v2_name <- cmp[[2]]

  names(v1_name) <- paste(names(v1_name), "x", sep = ".")
  names(v2_name) <- paste(names(v2_name), "y", sep = ".")
  v_name <- cbind(v1_name, v2_name)
  testthat::expect_equal(length(setdiff(names(v_name), dict_column_names)), 0)
  v_name
}
