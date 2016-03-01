test_that("linearized intensity is valid", {
  features <- c(
    "Intensity_first_quartile",
    "Intensity_integrated"
  )

  linearized <-
    linearize(
      population = fixture_intensities,
      variables = features,
      sample = fixture_intensities %>%
        dplyr::filter(well_description %in% c("A01", "A02")),
      lower_quantile = 0.25,
      upper_quantile = 0.75
    )

  a <- linearized %>% dplyr::select_(.dots = features) %>% as.matrix()
  b <- fixture_linearized_intensities %>% dplyr::select_(.dots = features) %>% as.matrix()

  expect_equal(a, b)
})
