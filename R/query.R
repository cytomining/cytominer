#' Query sim.mat
#'
#' @param S sim.mat object
#' @param query_frame data.frame with query in each row
#' @param ... additional parameters

query <- function(S, query_frame, ...)
  UseMethod("query")


#' @describeIn query Query sim.mat object
#'

query.sim.mat <- function(S, query_frame, ...) {
  testthat::expect_is(S, "sim.mat")
  testthat::expect_is(query_frame, "data.frame")

  # impose that the query be only one row because the logic is more
  # complicated with multiple rows
  # TODO: remove this constraint
  testthat::expect_equal(nrow(query_frame), 1)

  # get colnames of row and col portions of the query
  row_q_names_ <- stringr::str_subset(names(query_frame), ".x$")
  col_q_names_ <- stringr::str_subset(names(query_frame), ".y$")
  expect_true(setequal(c(row_q_names_, col_q_names_), names(query_frame)))

  # strip out .x and .y
  row_q_names <- stringr::str_replace(row_q_names_, ".x", "")
  col_q_names <- stringr::str_replace(col_q_names_, ".y", "")

  # test
  expect_true(all(row_q_names %in% names(row_meta(S))),
              info = paste(names(row_meta(S)), row_q_names, collapse=","))

  expect_true(all(col_q_names %in% names(col_meta(S))),
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
  futile.logger::flog.debug("Querying on rows...")
  row_res <- dplyr::left_join(row_q, S$row_meta)
  names(row_res) <- paste(names(row_res), "x", sep = ".")

  futile.logger::flog.debug("Querying on cols...")
  col_res <- dplyr::left_join(col_q, S$col_meta)
  names(col_res) <- paste(names(col_res), "y", sep = ".")

  full_res <- query_frame
  full_res <- dplyr::left_join(full_res, row_res)
  full_res <- dplyr::left_join(full_res, col_res)
  full_res %<>% dplyr::select_(.dots = c(names(query_frame), "Var1.x", "Var2.y"))
  futile.logger::flog.debug("Query result frame has %d rows", nrow(full_res))
  full_res %<>% na.omit()
  futile.logger::flog.debug("Final query result has %d rows", nrow(full_res))

  if (nrow(full_res) > 0) {
    full_res %<>% dplyr::rowwise() %>% dplyr::mutate(value = smat(S)[Var1.x, Var2.y])
#     futile.logger::flog.debug("Final query result = \n%s",
#                               paste(capture.output(full_res), collapse="\n"))
  }

  return (full_res)
}
