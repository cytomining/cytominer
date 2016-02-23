test_that("variance thresholded shapes is valid", {

  metadata_cols <- c("plate_barcode",
                     "well_description",
                     "image_description",
                     "object_description",
                     "pattern_description")

  feat_cols <- c("AreaShape_euler_number",
                 "AreaShape_compactness")

  variance_threshold_shapes <-
    variance_threshold(fixture_shapes %>%
                         dplyr::select_(.dots = feat_cols))

  expect_equal(
    variance_threshold_shapes %>% as.matrix(),
    fixture_variance_threshold_shapes %>%
      dplyr::select(-one_of(metadata_cols)) %>%
      as.matrix()
  )
})
