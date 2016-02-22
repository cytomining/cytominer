test_that("robustized intensity is valid", {

  metadata_cols <- c("plate_barcode",
                     "well_description",
                     "image_description",
                     "object_description",
                     "pattern_description",
                     "channel_description")

  feat_cols <- c("Intensity_first_quartile",
                 "Intensity_integrated")

  robustized <-
    robustize(fixture_intensities %>%
                dplyr::select_(.dots = feat_cols),
              fixture_intensities %>%
                dplyr::filter(well_description %in%
                                c("A01", "A02")) %>%
                dplyr::select_(.dots = feat_cols)
              )

  expect_equal(
    robustized %>% as.matrix(),
    fixture_robustized_intensities %>%
      dplyr::select_(.dots = feat_cols) %>%
      as.matrix()
    )
})
