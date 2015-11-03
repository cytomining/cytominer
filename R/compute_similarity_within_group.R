#' Similarity between between all pairs of vectors within a group
#'
#' Function to measure similarity between all pairs of vectors within a group
#'
#' @param P profile.data
#' @param grp data.frame specifying list of grouping variables
#' @param method type of similarity method
#' @param ... Arguments to be passed to methods
#'
#' @return list of sim.mats, one per group
#'
compute_similarity_within_group <-
  function(P, grp, method, ...) UseMethod("compute_similarity_within_group")

#' @describeIn compute_similarity_within_group Similarity between between all pairs of vectors within a group

compute_similarity_within_group.profile.data <- function (P, grp,
  method = "pearson") {

  testthat::expect_is(P, "profile.data")
  testthat::expect_is(grp, "character")
  testthat::expect_true(all(grp %in% names(meta(P))))

  grp_l <-
    meta(P) %>%
    dplyr::select_(.dots = grp) %>%
    dplyr::distinct() %>%
    dplyr::arrange_(.dots = grp)

  testthat::expect_more_than(nrow(grp_l), 0)

  futile.logger::flog.debug("Groups = %s", jsonlite::toJSON(grp_l))

  sim_l <- vector("list", length(grp_l))

  for (i in 1:nrow(grp_l)) {

    grp_i <- grp_l[i, , drop = F]
    testthat::expect_is(grp_i, "data.frame")
    testthat::expect_equal(names(grp_i), names(grp_l))
    futile.logger::flog.debug("Computing sim.mat for group = %s",
                               jsonlite::toJSON(grp_i))

    futile.logger::flog.debug("grp = %s", jsonlite::toJSON(grp_i))

    sim_i <- compute_similarity(P,
                                key1 = grp_i,
                                key2 = grp_i,
                                method = method,
                                return_index = T)

    futile.logger::flog.debug("Received %dx%d similarity matrix",
                              nrow(smat(sim_i)),
                              ncol(smat(sim_i)))

    sim_l[[i]] <- sim_i

  }
  futile.logger::flog.debug("sim_l = %.2f Mb",
                            pryr::object_size(sim_l) / (1024 ** 2))
  return(sim_l)
}
