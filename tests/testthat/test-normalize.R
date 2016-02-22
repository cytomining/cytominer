test_that("Normalizing intensity is valid", {
  expect_equal(nrow(fixture_intensities), 4959)
  normalized <- normalize(fixture_intensities,
                          fixture_intensities %>%
                            dplyr::filter(well_description %in%
                                            c("A01", "A02")))

})
