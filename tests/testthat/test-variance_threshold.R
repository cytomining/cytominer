test_that("variance thresholded shapes is valid", {

  metadata <- c("plate_barcode",
                "well_description",
                "image_description",
                "object_description",
                "pattern_description")

  features <- c("AreaShape_euler_number",
                 "AreaShape_compactness")

  a <- setdiff(colnames(fixture_shapes),
               variance_threshold(population = fixture_shapes,
                                  variables = features,
                                  sample = fixture_shapes))

  b <- fixture_variance_threshold_shapes %>% names()

  expect_equal(a, b)

  a <- variance_threshold(population = fixture_shapes,
                          variables = c("AreaShape_compactness"),
                          sample = fixture_shapes)

  expect_equal(length(a), 0)

})
