test_that("standardized intensity is valid", {

  metadata_cols <- c("plate_barcode",
                     "well_description",
                     "image_description",
                     "object_description",
                     "pattern_description",
                     "channel_description")

  features <- c("Intensity_first_quartile",
                 "Intensity_integrated")

  standardized <-
    standardize(population = fixture_intensities,
                variables = features,
                sample = fixture_intensities %>%
                  dplyr::filter(well_description %in%
                                  c("A01", "A02")))


  a <- standardized %>% dplyr::select_(.dots = features) %>% as.matrix()
  b <- fixture_standardized_intensities %>% dplyr::select_(.dots = features) %>% as.matrix()

  expect_equal(a, b)
})
