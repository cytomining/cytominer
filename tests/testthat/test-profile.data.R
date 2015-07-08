context("Load profile data")


test_that("Sample dataset loads correctly", {
  skip("Skipping because it is too slow")
  expect_is(
    profile.data("../../inst/extdata/well-summary-profile_mean-median-robust_std-untreated_norm.yml",
                 use_csv = TRUE),
    "profile.data")
})

D <- profile.data("../../inst/extdata/well-summary-profile_mean-median-robust_std-untreated_norm.yml")

test_that("Sample dataset has expected structure", {
  expect_is(D$metadata, "data.frame")
  expect_is(D$featdata, "data.frame")

})
