test_that("robustized intensity is valid", {
  features <- c(
    "Intensity_first_quartile",
    "Intensity_integrated"
  )

  robustized <-
    robustize(
      population = fixture_intensities,
      variables = features,
      sample = fixture_intensities %>%
        dplyr::filter(well_description %in% c("A01", "A02"))
    )

  a <- robustized %>% dplyr::select_(.dots = features) %>% as.matrix()
  b <- fixture_robustized_intensities %>% dplyr::select_(.dots = features) %>% as.matrix()

  expect_equal(a, b)
})
