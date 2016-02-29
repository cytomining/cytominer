test_that("correlation thresholded intensities is valid", {
  metadata_cols <- c(
    "plate_barcode",
    "well_description",
    "image_description",
    "object_description",
    "pattern_description",
    "channel_description"
  )

  feat_cols <- c(
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

  population <- fixture_intensities %>% dplyr::select_(.dots = feat_cols)

  sample <- population

  correlation_threshold_intensities <- correlation_threshold(
    population = population,
    sample = sample
  )

  a <- correlation_threshold_intensities %>% as.matrix()

  b <- fixture_correlation_threshold_intensities %>% dplyr::select(-one_of(metadata_cols)) %>% as.matrix()

  expect_equal(a, b)
})
