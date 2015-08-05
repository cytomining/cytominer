#' Access metadata
#' @param ... object from which to extract metadata
meta  <- function(...) UseMethod("meta")

#' Access featdata
#' @param ... object from which to extract featdata
feat  <- function(...) UseMethod("feat")

#' Access merged featdata and metadata
#' @param ... object from which to extract merged featdata and metadata
full  <- function(...) UseMethod("full")

#' @describeIn meta
#' @param P profile.data object
meta.profile.data <- function(P, ...) P$metadata[names(P$metadata) != "xid"]

#' @describeIn feat
#' @param P profile.data object
feat.profile.data <- function(P, ...) P$featdata[names(P$featdata) != "xid"]

#' @describeIn full
#' @param P profile.data object
#' @param keep_xid if True, don't exclude xid in the result
full.profile.data <- function(P, keep_xid = F, ...) {
  if(keep_xid) {
    return(merge_by_xid(P$metadata, P$featdata))
  } else {
    return(merge_by_xid(P$metadata, P$featdata) %>% dplyr::select(-xid))
  }
}
