#' Replace metadata
#'
#' @param P profile.data
#' @param meta_new data.frame with new metadata columns
#' @param key_cols key columns on which to join
#' @param ... additional parameters

replace_metadata <- function(P, ...)
  UseMethod("replace_metadata")


#' @describeIn replace_metadata Replace metadata in a profile.data object

replace_metadata.profile.data <- function(P, meta_new, key_cols, ...) {

  meta_old <- meta(P)

  # Tests
  testthat::expect_true(all(key_cols %in% names(meta_new)))
  testthat::expect_true(all(names(meta_new) %in% names(meta_old)))
  testthat::expect_equal(meta_new %>%
                           dplyr::distinct() %>%
                           dplyr::group_by_(.dots = key_cols) %>%
                           dplyr::summarize(count = n()) %>%
                           dplyr::filter(count != 1) %>%
                           nrow(), 0)

  # Get the columns corresponding to the values
  val_cols = setdiff(names(meta_new), key_cols)
  testthat::expect_more_than(length(val_cols), 0)

  # Prune the original metadata down to only the columns that are in
  # meta_new and then select unique rows. This is to get the
  # key-value pairs from the metadata
  meta_old %<>%
    dplyr::select_(.dots = names(meta_new)) %>%
    dplyr::distinct()

  # Check if the original metadata has unique keys, if not display a
  # warning. This doesn't interfere with the replacement because
  # the old values will be discarded. However, if there are
  # duplicate keys that are not going to be replaced (because they are
  # not present in meta_new), this warning will help catch that
  tbl <- meta_old %>%
    dplyr::group_by_(.dots = key_cols) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::filter(count != 1)

  if (nrow(tbl) > 0) {
    futile.logger::flog.warn("Original metadata contains %d duplicate keys",
                             nrow(tbl))
    futile.logger::flog.warn("Duplicates = %s", jsonlite::toJSON(tbl))
  }

  # Join meta_new with original metadata
  # TODO: This throws a warning "joining character vector and factor, coercing
  # into character vector"
  meta_mrg <-
    meta_old %>%
    dplyr::left_join(meta_new, by = key_cols)

  # Merged metadata should have exactly the same number of rows
  testthat::expect_equal(nrow(meta_mrg), nrow(meta_old))

  # Check column names of merged metadata
  testthat::expect_equal(
    stringr::str_subset(names(meta_mrg), "\\.[xy]$") %>% sort(),
    c(stringr::str_c(val_cols, ".x"), stringr::str_c(val_cols, ".y")) %>% sort()
  )

  # Iterate over the value columns and replace values
  for (val_col in val_cols) {
    x_var <- sprintf("%s.x", val_col)
    y_var <- sprintf("%s.y", val_col)
    repl_str <- setNames(list(sprintf("ifelse(is.na(%s), %s, %s)",
                                      y_var, x_var, y_var)), val_col)
    meta_mrg %<>% dplyr::mutate_(.dots = repl_str)
  }

  # Select only the columns corresponding to the key-value pairs
  meta_mrg %<>%
    dplyr::select_(.dots = names(meta_old)) %>%
    dplyr::distinct()

  # test
  testthat::expect_equal(nrow(meta_mrg), nrow(unique(meta_old[key_cols])))

  # remove the key columns
  P$metadata <- P$metadata[, setdiff(names(P$metadata), val_cols)]

  # replace with the new metadata
  P %<>% add_metadata(meta_mrg, key_cols)

  return(P)
}
