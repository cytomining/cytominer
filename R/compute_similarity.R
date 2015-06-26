#' Similarity between two vectors
#'
#' Function to measure similarity between two vectors
#'
#' @param D1 data.frame
#' @param D2 same dimensions as \code{D1}
#'
#' @return Similarity between D1 and D2
#'
##' @examples
##' compute_similarity(c(1,2,3,4,5), c(1,2,4,3,5))
#'
compute_similarity <- function(D1,
                               D2,
                               grouping_cols = NULL,
                               method = "spearman") {

  D1_mat <- as.matrix(D1[,setdiff(names(D1), grouping_cols)])
  D2_mat <- as.matrix(D2[,setdiff(names(D2), grouping_cols)])

  cor(t(D1_mat), t(D2_mat))

}

