test_that("variance thresholded shapes is valid", {

  metadata <- c("plate_barcode",
                "well_description",
                "image_description",
                "object_description",
                "pattern_description")

  features <- c("AreaShape_euler_number",
                 "AreaShape_compactness")

  variance_threshold_shapes <-
    variance_threshold(population = fixture_shapes,
                       variables = features,
                       sample = fixture_shapes)

  a <- variance_threshold_shapes %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  b <- fixture_variance_threshold_shapes %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  expect_equal(a, b)

})
