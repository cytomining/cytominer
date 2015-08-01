#' Query sim.mat
#'
#' @param S sim.mat object
#' @param query_frame data.frame with query in each row
#' @param ... additional parameters
#'
query <- function(S, query_frame, ...)
  UseMethod("query")


#' @describeIn query Query sim.mat object
#'
#' @param return_all_cols If True, returns all columns of the query result,
#' else returns only the columns that were present in the query
#'
#' @return data.frame of query result. The similarity value is stored in "value"
#'
query.sim.mat <- function(S, query_frame, return_all_cols = F, ...) {
  testthat::expect_is(S, "sim.mat")
  testthat::expect_is(query_frame, "data.frame")

  # impose that the query be only one row because the logic is more
  # complicated with multiple rows
  # TODO: remove this constraint
  testthat::expect_equal(nrow(query_frame), 1,
                         info  = "query be only one row because the logic is more complicated with multiple rows")

  # get colnames of row and col portions of the query
  row_q_names_ <- stringr::str_subset(names(query_frame), ".x$")
  col_q_names_ <- stringr::str_subset(names(query_frame), ".y$")
  testthat::expect_true(setequal(c(row_q_names_, col_q_names_), names(query_frame)))

  # strip out .x and .y
  row_q_names <- stringr::str_replace(row_q_names_, ".x", "")
  col_q_names <- stringr::str_replace(col_q_names_, ".y", "")

  # test
  testthat::expect_true(all(row_q_names %in% names(row_meta(S))),
                        info = paste(names(row_meta(S)), row_q_names, collapse=","))

  testthat::expect_true(all(col_q_names %in% names(col_meta(S))),
                        info = paste(names(col_meta(S)), col_q_names, collapse=","))

  # extract the row query
  futile.logger::flog.debug("Extracting row query...")
  row_q <- query_frame[row_q_names_]
  names(row_q) <- row_q_names

  # extract the col query
  futile.logger::flog.debug("Extracting col query...")
  col_q <- query_frame[col_q_names_]
  names(col_q) <- col_q_names

  # get the row and col query result
  # (don't use row_meta() and col_meta() because we want the index)
  # left_join: return all rows from x, and all columns from x and y. Rows in x
  # with no match in y will have NA values in the new columns. If there are
  # multiple matches between x and y, all combinations of the matches are
  # returned.

  futile.logger::flog.debug("Querying on rows...")
  row_res <- dplyr::left_join(row_q, S$row_meta)

  futile.logger::flog.debug("Querying on cols...")
  col_res <- dplyr::left_join(col_q, S$col_meta)

  # Merge the row and col results
  # start with the query frame
  full_res <- query_frame
  # rename the row and col result colnames so that the end with the
  # corresponding suffix in the query frame
  names(row_res) <- paste(names(row_res), "x", sep = ".")
  names(col_res) <- paste(names(col_res), "y", sep = ".")
  # now do a left join of the query frame (stored in full_res) with the row
  # result so that all the row results are present
  full_res <- dplyr::left_join(full_res, row_res)
  # next do a left join on of the full_res with the col result so that all the
  # col results are present.
  full_res <- dplyr::left_join(full_res, col_res)

  if (!return_all_cols) {
    # Preserve only a few columns of the full_res. Var1.x and Var2.y store the i,j
    # index of the similarity matrix corresponding to the result
    full_res %<>% dplyr::select_(.dots = c(names(query_frame), "Var1.x", "Var2.y"))
  }
  futile.logger::flog.debug("Query result frame has %d rows", nrow(full_res))

  # Rows in query with no match in  will have NA values in either the columns
  # corresponding to either or both of the row result col result.
  # Drop these rows
  full_res %<>% na.omit()
  futile.logger::flog.debug("Final query result has %d rows", nrow(full_res))

  if (nrow(full_res) > 0) {
    # Now look up the i,j values in the simililarity matrix and append it to the
    # full_res matrix
    futile.logger::flog.debug("Appending values from smat...")
    smat_ <- smat(S)
    full_res %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = smat_[Var1.x, Var2.y]) %>%
      dplyr::select(-Var1.x, -Var2.y) %>%
      dplyr::ungroup()
    futile.logger::flog.debug("Finished appending values from smat.")
    full_res_str <- paste(capture.output(full_res), collapse="\n")
    #futile.logger::flog.debug("Final query result = \n%s", full_res_str)
  }

  return (full_res)
}
