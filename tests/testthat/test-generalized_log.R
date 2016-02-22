test_that("generalized log of intensity is valid", {

  metadata_cols <- c("plate_barcode",
                     "well_description",
                     "image_description",
                     "object_description",
                     "pattern_description",
                     "channel_description")

  feat_cols <- c("Intensity_first_quartile",
                 "Intensity_integrated")

  generalized_log_intensitites <-
    generalized_log(fixture_intensities_small %>%
                      dplyr::slice(1:4) %>%
                      dplyr::select_(.dots = feat_cols))

  expect_equal(
    generalized_log_intensitites %>% as.matrix(),
    fixture_generalized_log_intensities %>%
      dplyr::select_(.dots = feat_cols) %>%
      as.matrix()
    )
})
