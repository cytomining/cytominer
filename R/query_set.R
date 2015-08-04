#' Query list of sim.mats
#'
#' This is a wrapper for query() that accepts multiple rows in the query frame
#'
#' @param S_l list of sim.mats
#' @param query_frame data.frame with query in each row
#' @param ... additional parameters

query_set <- function(S_l, query_frame, ...)
  UseMethod("query_set")

#' @describeIn query_set Query set of sim.mats
#'
#' @param return_all_cols If True, returns all columns of the query result,
#' else returns only the columns that were present in the query
#'
#' @return data.frame with query result
#'
query_set.list <- function(S_l, query_frame, return_all_cols = F, ...) {
  testthat::expect_is(S_l, "list")
  testthat::expect_is(query_frame, "data.frame")
  testthat::expect_more_than(nrow(query_frame), 0)
  lapply(S_l, function(S) testthat::expect_is(S, "sim.mat"))

  futile.logger::flog.debug("Query frame has %d rows", nrow(query_frame))
  futile.logger::flog.debug("S_l has %d sim.mats", length(S_l))

  if (nrow(query_frame) == 1) {
    query_f <- query.sim.mat
  } else {
    query_f <- query_n.sim.mat
  }

  res <- vector("list", length(S_l))

  for(i in seq(length(S_l))) {
    S <- S_l[[i]]
    futile.logger::flog.debug("Querying sim.mat #%d which is %dx%d ...",
                              i,
                              dim(smat(S))[1],
                              dim(smat(S))[2])
    res[[i]] <- query_f(S,  query_frame, return_all_cols = return_all_cols)
    futile.logger::flog.debug("Query returned %d rows", nrow(res[[i]]))
  }

  res <- do.call("rbind", res)
  futile.logger::flog.debug("Query across all sets returned %d rows in total",
                            nrow(res))

  return(res)

}
