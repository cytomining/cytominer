test_that("correlation thresholded intensities is valid", {

  metadata <- c("plate_barcode",
                "well_description",
                "image_description",
                "object_description",
                "pattern_description",
                "channel_description")

  features <- c(
    "Intensity_first_quartile",
    "Intensity_integrated",
    "Intensity_maximum",
    "Intensity_mean",
    "Intensity_median",
    "Intensity_median_absolute_deviation",
    "Intensity_minimum",
    "Intensity_standard_deviation",
    "Intensity_third_quartile"
  )

  a <- setdiff(colnames(fixture_intensities),
               correlation_threshold(population = fixture_intensities,
                                     variables = features,
                                     sample = fixture_intensities))

  b <- fixture_correlation_threshold_intensities %>% names()

  expect_equal(a, b)

  a <- correlation_threshold(population = fixture_intensities,
                             variables = c("Intensity_first_quartile", "Intensity_integrated"),
                             sample = fixture_intensities)

  expect_equal(length(a), 0)
})
