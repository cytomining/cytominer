#' Similarity between two sets of vectors
#'
#' Function to measure similarity between two sets of vectors
#'
#' @param D1 data.frame
#' @param D2 same dimensions as \code{D1}
#' @param grouping_cols list columns to be used as metadata
#' @param melt If True, return the similarity matrix as a melted data frame, joined with grouping columns
#'
#' @return Similarity between \code{D1} and \code{D2}
#'
compute_similarity <- function(D1,
                               D2,
                               grouping_cols = NULL,
                               method = "spearman",
                               melt = F) {

  testthat::expect_true(all(grouping_cols %in% names(D1)))
  testthat::expect_true(all(grouping_cols %in% names(D2)))

  D1_mat <- as.matrix(D1[,setdiff(names(D1), grouping_cols)])
  D2_mat <- as.matrix(D2[,setdiff(names(D2), grouping_cols)])

  testthat::expect_true(all(names(D1_mat)==names(D1_mat)))

  sim_mat <- cor(t(D1_mat), t(D2_mat), method = method)

  if (!melt) {
    return(sim_mat)
  } else {
    testthat::expect_false("Var1" %in% grouping_cols)
    testthat::expect_false("Var2" %in% grouping_cols)
    colnames(sim_mat) <- seq(NCOL(sim_mat))
    row.names(sim_mat) <- seq(NROW(sim_mat))
    sim_mat_m <- reshape2::melt(sim_mat)
    sim_mat_m_nrow <- nrow(sim_mat_m)
    testthat::expect_true(setequal(names(sim_mat_m), c("Var1", "Var2", "value")))
    D1_grouping_cols <- D1[,grouping_cols]
    D2_grouping_cols <- D2[,grouping_cols]
    names(D1_grouping_cols) <- paste(names(D1_grouping_cols), "x", sep = ".")
    names(D2_grouping_cols) <- paste(names(D2_grouping_cols), "y", sep = ".")
    D1_grouping_cols$Var1 <- seq(NROW(D1))
    D2_grouping_cols$Var2 <- seq(NROW(D2))
    sim_mat_m <- dplyr::inner_join(sim_mat_m, D1_grouping_cols, by = c("Var1"))
    sim_mat_m <- dplyr::inner_join(sim_mat_m, D2_grouping_cols, by = c("Var2"))
    sim_mat_m$Var1 <- NULL
    sim_mat_m$Var2 <- NULL
    testthat::expect_equal(nrow(sim_mat_m), sim_mat_m_nrow)
    return(sim_mat_m)
  }

}

