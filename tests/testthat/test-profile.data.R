context("Load and do some basic processing on a profile.data object")

cfg_fname <-
  system.file("extdata",
              "well-summary-profile_mean-median-robust_std-untreated_norm.yml",
              package = "pertminr")

test_that("Sample dataset loads correctly", {
  skip("Skipping because it is too slow")
  expect_is(profile.data(cfg_fname, use_csv = TRUE),
            "profile.data")
})

P <- profile.data(cfg_fname)

test_that("Sample dataset has expected structure", {
  expect_is(meta(P), "data.frame")
  expect_is(feat(P), "data.frame")
})

test_that("Processing metadata has expected behavior", {
  expect_is(
    process_metadata(P, strip_cellprofiler_db_tags = T),
    "profile.data"
  )
})

test_that("process_metadata returns a valid profile.data object", {
  expect_is(process_metadata(P, strip_cellprofiler_db_tags = T),
            "profile.data")
})

P <- process_metadata(P, strip_cellprofiler_db_tags = T)
P$metadata <- dplyr::filter(P$metadata, Plate %in% c(38034, 38003, 37983))

test_that("post_filter_metadata returns a valid profile.data object", {
  expect_is(post_filter_metadata(P), "profile.data")
})

P <- post_filter_metadata(P)
