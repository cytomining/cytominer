#' Filter rows of featdata by joining with metadata
#'
#' Used after metadata has been filtered
#'
#' @param P profile.data
#' @param ... Arguments to be passed to methods
#'
#' @return profile.data, after filtering featdata
#' @importFrom dplyr '%>%'

post_filter_metadata <- function(...) UseMethod("post_filter_metadata")

#' @describeIn post_filter_metadata
#'
post_filter_metadata.profile.data <- function(P, ...) {
  testthat::expect_is(P, "profile.data")
  testthat::expect_true("xid" %in% names(P$metadata))
  testthat::expect_true("xid" %in% names(P$featdata))
  testthat::expect_equal(length(setdiff(P$metadata$xid, P$featdata$xid)), 0)

  xid_l <- P$metadata$xid
  P$featdata <- dplyr::filter(P$featdata, xid %in% xid_l)

  testthat::expect_true(setequal(P$metadata$xid, P$featdata$xid))
  P
}
