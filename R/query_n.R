#' Query sim.mat - multiple queries are allowed
#'
#' This is a wrapper for query() that accepts multiple rows in the query frame
#'
#' @param S sim.mat object
#' @param query_frame data.frame with query in each row
#' @param ... additional parameters

query_n <- function(S, query_frame, ...)
  UseMethod("query_n")


#' @describeIn query_n Query sim.mat object - multiple queries are allowed
#'
#' @param return_all_cols If True, returns all columns of the query result,
#' else returns only the columns that were present in the query
#'
#' @return data.frame with query result
#'
query_n.sim.mat <- function(S, query_frame, return_all_cols = F, ...) {
  testthat::expect_is(S, "sim.mat")
  testthat::expect_is(query_frame, "data.frame")
  futile.logger::flog.debug("Query frame has %d rows", nrow(query_frame))

  res <- vector("list", nrow(query_frame))

  for(i in seq(nrow(query_frame))) {
    res[[i]] <- query(S,  query_frame[i, ], return_all_cols = return_all_cols)
    if (i == 1) {
      res_names <- names(res[i])
      flag_res_names_equal <- TRUE
    } else {
      flag_res_names_equal <- flag_res_names_equal &
        setequal(names(res[i]), res_names)
    }
  }

  if (!flag_res_names_equal) {
    for(i in seq(nrow*query_frame)) {
      res[i] <- res[i]["value"]
    }
  }
  res <- do.call("rbind", res)

}
