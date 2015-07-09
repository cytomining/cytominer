#' Process metadata
#'
#' @param P profile.data
#' @param strip_cellprofiler_db_tags If True, drop the leading characters
#' in the metadata columns names that originate from the per_image table
#' of the CellProfiler output
#'

process_metadata <- function(P, strip_cellprofiler_db_tags = T)
  UseMethod("process_metadata")


#' @describeIn process_metadata Process metadata for profile.data object

process_metadata.profile.data <- function(P, strip_cellprofiler_db_tags = T) {
  testthat::expect_is(P, "profile.data")
  testthat::expect_is(P$cfg, "list")
  testthat::expect_is(P$cfg$mapping, "list")
  testthat::expect_is(P$cfg$mapping$dbname, "character")
  testthat::expect_is(P$cfg$mapping$metadata_tag, "character")

  metadata_names <- names(P$metadata)
  metadata_names <- gsub(P$cfg$mapping$metadata_tag,
                         '', metadata_names)
  metadata_names <- gsub(paste(P$cfg$mapping$dbname, "", sep="."),
                         '', metadata_names)
  testthat::expect_equal(length(metadata_names), length(names(P$metadata)))
  names(P$metadata) <- metadata_names
  P

}


