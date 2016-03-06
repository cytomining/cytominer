# TODO: these tests are not great because ideally grouping_cols should have more
# than one element in order to test it out

test_that("standardized intensity is valid", {

  grouping_cols <- c("plate_barcode")

  features <- c("Intensity_first_quartile",
                "Intensity_integrated")

  standardized <-
    normalize(population = fixture_intensities,
              variables = features,
              grouping_variables = grouping_cols,
              sample = fixture_intensities %>%
                dplyr::filter(well_description %in% c("A01", "A02")),
              operation = "standardize")


  a <- standardized %>% dplyr::select_(.dots = features) %>% as.matrix()
  b <- fixture_standardized_intensities %>% dplyr::select_(.dots = features) %>% as.matrix()

  expect_equal(a, b)
})

test_that("robustized intensity is valid", {

  grouping_cols <- c("plate_barcode")

  features <- c("Intensity_first_quartile",
                "Intensity_integrated")

  robustized <-
    normalize(population = fixture_intensities,
              variables = features,
              grouping_variables = grouping_cols,
              sample = fixture_intensities %>%
                dplyr::filter(well_description %in% c("A01", "A02")),
              operation = "robustize")


  a <- robustized %>% dplyr::select_(.dots = features) %>% as.matrix()
  b <- fixture_robustized_intensities %>% dplyr::select_(.dots = features) %>% as.matrix()

  expect_equal(a, b)
})
