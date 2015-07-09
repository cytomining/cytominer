#' Access metadata
#' @param ... object from which to extract metadata
meta  <- function(...) UseMethod("meta")

#' Access featdata
#' @param ... object from which to extract featdata
feat  <- function(...) UseMethod("feat")

#' @describeIn meta
#' @param P profile.data object
meta.profile.data <- function(P, ...) P$metadata

#' @describeIn feat
#' @param P profile.data object
feat.profile.data <- function(P, ...) P$featdata
