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
  testthat::expect_true(all(names(meta(P_new)) %in% names(meta(P))),
                        info = stringr::str_c(setdiff(names(meta(P_new)), names(meta(P))), collapse = ","))
  testthat::expect_equal(meta(P_new) %>% dplyr::distinct() %>% nrow(),
                         meta(P_new) %>% nrow())
  common_feat_columns <- intersect(names(feat(P)), names(feat(P_new)))
  testthat::expect_equal(length(common_feat_columns), 0)

  futile.logger::flog.debug("These columns will be used to join the metadata: %s",
                            stringr::str_c(names(meta(P_new)), collapse = ","))
  meta_old <- P$metadata
  # Get full data of the new profile.data
  full_new <- full(P_new, keep_xid = F)
  testthat::expect_equal(intersect(names(meta_old), names(full_new)) %>% sort(),
                         intersect(names(meta_old), names(meta(P_new))) %>% sort()
                        )
  # Merge old metadata with the new unified data
  full_new <- dplyr::left_join(meta_old, full_new, by = names(meta(P_new)))
  testthat::expect_equal(nrow(full_new), nrow(meta_old))

  # Select only the feat columns
  feat_new <- full_new %>% dplyr::select_(.dots = c("xid", names(feat(P_new))))
  testthat::expect_equal(anyDuplicated(feat_new$xid), 0)

  # Update featdata
  P$featdata %<>% dplyr::left_join(feat_new, by = "xid")

  # check that the xids are still ok
  testthat::expect_true(setequal(P$featdata$xid, P$metadata$xid))
  P
}
