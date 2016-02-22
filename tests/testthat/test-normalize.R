test_that("Normalizing intensity is valid", {
  expect_equal(nrow(fixture_intensities), 4959)
  expect_equal(nrow(fixture_normalized_intensities), 4959)
})
