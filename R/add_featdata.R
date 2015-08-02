#' Add featdata
#'
#' @param P profile.data
#' @param ... additional parameters

add_featdata <- function(P, ...)
  UseMethod("add_featdata")


#' @describeIn add_featdata Add featdata to profile.data object
#'
#' the metadata of P_new will be used join.
#'
#' @param P_new profile.data with new featdata columns

add_featdata.profile.data <- function(P, P_new, ...) {
  testthat::expect_is(P, "profile.data")
  testthat::expect_is(P_new, "profile.data")
  testthat::expect_true(all(names(meta(P_new)) %in% names(meta(P))))
  common_feat_columns <- intersect(names(feat(P)), names(feat(P_new)))
  testthat::expect_equal(length(common_feat_columns), 0)

  meta_old <- meta(P)
  # Get unified data of the new profile.data so that
  data_new <- merge_by_xid(P_new$metadata, P_new$featdata)
  testthat::expect_equal(intersect(names(meta_old), names(data_new)) %>% sort(),
                         intersect(names(meta_old), names(meta(P_new))) %>% sort()
                        )
  # Merge old metadata with the new unified data
  data_new <- dplyr::left_join(meta_old, data_new)
  testthat::expect_equal(nrow(data_new), nrow(meta_old))

  # Select only the feat columns
  feat_new <- data_new %>% dplyr::select_(.dots = c("xid", names(feat(P_new))))
  testthat::expect_equal(anyDuplicated(feat_new$xid), 0)

  # Update featdata
  P$featdata %<>% dplyr::left_join(feat_new, by = "xid")

  # check that the xids are still ok
  testthat::expect_true(setequal(P$featdata$xid, P$metadata$xid))
  P
}
