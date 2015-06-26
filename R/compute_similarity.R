#' Similarity between two vectors
#'
#' Function to measure similarity between two vectors
#'
#' @param x vector
#' @param y vector of the same length as x
#'
#' @return Similarity between x and y
#'
#' @examples
#' compute_similarity(c(1,2,3,4,5), c(1,2,4,3,5))
compute_similarity <- function(x, y) {
  cor(x, y)
}

