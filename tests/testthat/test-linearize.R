test_that("linearized intensity is valid", {

  metadata_cols <- c("plate_barcode",
                     "well_description",
                     "image_description",
                     "object_description",
                     "pattern_description",
                     "channel_description")

  feat_cols <- c("Intensity_first_quartile",
                 "Intensity_integrated")

  linearized <-
    linearize(fixture_intensities %>%
                dplyr::select_(.dots = feat_cols),
              fixture_intensities %>%
                dplyr::filter(well_description %in%
                                c("A01", "A02")) %>%
                dplyr::select_(.dots = feat_cols)
              )

  expect_equal(
    linearized %>% as.matrix(),
    fixture_linearized_intensities %>%
      dplyr::select_(.dots = feat_cols) %>%
      as.matrix()
    )
})
