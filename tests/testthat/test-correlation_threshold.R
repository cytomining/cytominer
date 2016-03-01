test_that("correlation thresholded intensities is valid", {

  metadata <- c("plate_barcode",
                "well_description",
                "image_description",
                "object_description",
                "pattern_description",
                "channel_description")

  features <- c(
    'Intensity_first_quartile',
    'Intensity_integrated',
    'Intensity_maximum',
    'Intensity_mean',
    'Intensity_median',
    'Intensity_median_absolute_deviation',
    'Intensity_minimum',
    'Intensity_standard_deviation',
    'Intensity_third_quartile'
  )

  correlation_threshold_intensities <-
    correlation_threshold(population = fixture_intensities,
                          variables = features,
                          sample = fixture_intensities)

  a <- correlation_threshold_intensities %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  b <- fixture_correlation_threshold_intensities %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  expect_equal(a, b)
})
