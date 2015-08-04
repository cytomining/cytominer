#' Access row metadata
#' @param ... object from which to extract row metadata
row_meta  <- function(...) UseMethod("row_meta")

#' Access col metadata
#' @param ... object from which to extract col metadata
col_meta  <- function(...) UseMethod("col_meta")

#' Access similarity matrix
#' @param ... object from which to extract similarity matrix
smat  <- function(...) UseMethod("smat")

#' @describeIn row_meta
#' @param S sim.mat object
row_meta.sim.mat <- function(S, ...) S$row_meta[, names(S$row_meta) != "Var1"]

#' @describeIn col_meta
#' @param S sim.mat object
col_meta.sim.mat <- function(S, ...) S$col_meta[, names(S$col_meta) != "Var2"]

#' @describeIn smat
#' @param S sim.mat object
smat.sim.mat <- function(S, ...) S$smat
