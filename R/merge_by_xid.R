#' Join metadata and featdata
#'
#' @param metadata data.frame
#' @param featdata data.frame
#'
merge_by_xid <- function(metadata, featdata) {
  testthat::expect_is(metadata, "data.frame")
  testthat::expect_is(featdata, "data.frame")
  testthat::expect_true("xid" %in% names(metadata))
  testthat::expect_true("xid" %in% names(featdata))
  dplyr::inner_join(metadata, featdata, by = "xid")

}
