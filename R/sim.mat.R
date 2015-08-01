#' Constructor for sim.mat S3 class
#'
#' @param smat Similarity matrix
#' @param row_meta data.frame col metadata
#' @param col_meta data.frame col metadata
#'
#' @return sim.mat object
#'

sim.mat <- function(smat, row_meta, col_meta) {
  # Create sim.mat object
  # sim.mat assumes that the row_meta and col_meta will never be changed
  # It relies of the order of the rows of smat to be the same as that of rows
  # in row_meta, and the order of the columns of smat to be the same as that of
  # the rows in col_meta. This could lead to inconsistency
  row_meta$Var1 <- seq(nrow(row_meta))
  col_meta$Var2 <- seq(nrow(col_meta))

  obj <- list(smat = smat,
              row_meta = row_meta,
              col_meta = col_meta)

  class(obj) <- "sim.mat"

  obj
}
