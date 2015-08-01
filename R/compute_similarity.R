#' Similarity between two sets of vectors
#'
#' Function to measure similarity between two sets of vectors
#'
#' @param method type of similarity method
#' @param melt If True, return the similarity matrix as a melted data frame, joined with grouping columns
#' @param return_index If True, return the similarity matrix along with row and col metadata
#' @param ... Arguments to be passed to methods
#'
compute_similarity <- function(...) UseMethod("compute_similarity")

#' @describeIn compute_similarity Measure similarity between data frames
#'
#' @param D1 data.frame
#' @param D2 same dimensions as \code{D1}
#' @param grouping_cols list columns to be used as metadata
#'
#' @return Similarity between \code{D1} and \code{D2}
#'
compute_similarity.data.frame <- function(D1,
                                          D2,
                                          grouping_cols = NULL,
                                          method = "spearman",
                                          melt = F,
                                          return_index = F,
                                          ...) {

  testthat::expect_true(all(grouping_cols %in% names(D1)))
  testthat::expect_true(all(grouping_cols %in% names(D2)))
  testthat::expect_false(melt & return_index)

  D1_mat <- as.matrix(D1[,setdiff(names(D1), grouping_cols)])
  D2_mat <- as.matrix(D2[,setdiff(names(D2), grouping_cols)])

  testthat::expect_is(D1_mat, "matrix")
  testthat::expect_is(D2_mat, "matrix")

  testthat::expect_true(all(names(D1_mat)==names(D1_mat)))

  futile.logger::flog.debug("Computing %s between D1 (n=%d) and D2 (n=%d) ...",
                            method, nrow(D1), nrow(D2))

  sim_mat <- cor(t(D1_mat), t(D2_mat), method = method)

  if (!melt & !return_index) {
    futile.logger::flog.debug("Returning %dx%d similarity matrix",
                              nrow(sim_mat), ncol(sim_mat))
    return(sim_mat)
  } else if (!melt & return_index) {
    sim_mat_obj <- sim.mat(sim_mat, D1[,grouping_cols], D2[,grouping_cols])

    futile.logger::flog.debug("Returning %dx%d similarity matrix along with metadata",
                              nrow(sim_mat), ncol(sim_mat))
    return(sim_mat_obj)
  } else {
    testthat::expect_false("Var1" %in% grouping_cols)
    testthat::expect_false("Var2" %in% grouping_cols)
    colnames(sim_mat) <- seq(NCOL(sim_mat))
    row.names(sim_mat) <- seq(NROW(sim_mat))

    futile.logger::flog.debug("Melting %dx%d similarity matrix ...", nrow(sim_mat), nrow(sim_mat))

    sim_mat_m <- reshape2::melt(sim_mat)
    sim_mat_m_nrow <- nrow(sim_mat_m)
    testthat::expect_true(setequal(names(sim_mat_m), c("Var1", "Var2", "value")))

    futile.logger::flog.debug("Melted similarity matrix has %d rows", nrow(sim_mat_m))

    D1_grouping_cols <- D1[,grouping_cols]
    D2_grouping_cols <- D2[,grouping_cols]
    names(D1_grouping_cols) <- paste(names(D1_grouping_cols), "x", sep = ".")
    names(D2_grouping_cols) <- paste(names(D2_grouping_cols), "y", sep = ".")
    D1_grouping_cols$Var1 <- seq(NROW(D1))
    D2_grouping_cols$Var2 <- seq(NROW(D2))

    futile.logger::flog.debug("Binding %d columns of metadata to similarity matrix with %d rows ...",
                              ncol(D1_grouping_cols),
                              nrow(sim_mat_m))
    sim_mat_m <- dplyr::inner_join(sim_mat_m, D1_grouping_cols, by = c("Var1"))

    futile.logger::flog.debug("Binding %d columns of metadata to similarity matrix with %d rows ...",
                              ncol(D2_grouping_cols),
                              nrow(sim_mat_m))
    sim_mat_m <- dplyr::inner_join(sim_mat_m, D2_grouping_cols, by = c("Var2"))

    sim_mat_m$Var1 <- NULL
    sim_mat_m$Var2 <- NULL
    testthat::expect_equal(nrow(sim_mat_m), sim_mat_m_nrow)

    futile.logger::flog.debug("Returning %dx%d melted similarity matrix",
                              nrow(sim_mat_m), ncol(sim_mat_m))

    return(sim_mat_m)
  }

}

#' @describeIn compute_similarity Measure similarity between profile.data
#' objects
#'
#' @param P profile.data object
#' @param key1 first key
#' @param key2 second key

compute_similarity.profile.data <- function(P, key1, key2,
                                            method = "spearman",
                                            melt = F,
                                            return_index = F,
                                            ...) {

  testthat::expect_is(P, "profile.data")
  testthat::expect_equal(length(setdiff(names(key1), names(P$metadata))), 0)
  testthat::expect_equal(length(setdiff(names(key2), names(P$metadata))), 0)

  test_and_process_key <- function(key) {
    for (v in names(key)) {
      if (is.factor(P$metadata[, v])) {
        testthat::expect_true(key[, v] %in% levels(P$metadata[, v]))
        key[, v] <- factor(key[, v], levels = levels(P$metadata[, v]))
      }
    }
    key
  }
  key1 <- test_and_process_key(key1)
  key2 <- test_and_process_key(key2)

  futile.logger::flog.debug("key1 = %s", jsonlite::toJSON(key1))
  futile.logger::flog.debug("key2 = %s", jsonlite::toJSON(key2))

  D1 <- merge_by_xid(dplyr::inner_join(P$metadata, key1, by = names(key1)),
                                   P$featdata)

  futile.logger::flog.debug("Querying on key1 gives %d rows", nrow(D1))

  D2 <- merge_by_xid(dplyr::inner_join(P$metadata, key2, by = names(key2)),
                                   P$featdata)

  futile.logger::flog.debug("Querying on key2 gives %d rows", nrow(D2))

  return(compute_similarity.data.frame(D1, D2,
                                       grouping_cols = names(P$metadata),
                                       method = method,
                                       melt = melt,
                                       return_index = return_index)
         )
}


