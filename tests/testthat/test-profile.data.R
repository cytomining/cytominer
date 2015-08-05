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
  expect_is(full(P), "data.frame")
  expect_equal(nrow(meta(P)), nrow(feat(P)))
  expect_equal(nrow(full(P)), nrow(feat(P)))
  expect_equal(ncol(full(P)), ncol(feat(P)) + ncol(meta(P)))
  expect_true("xid" %in% names(full(P, keep_xid = T)))
  expect_false("xid" %in% names(full(P, keep_xid = F)))
})

test_that("Features in sample dataset are all numeric", {
  expect_true(is.numeric(as.matrix(feat(P))))
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
P$metadata %<>% dplyr::filter(Plate %in% c(38034, 38003, 37983))

test_that("post_filter_metadata returns a valid profile.data object", {
  expect_is(post_filter_metadata(P), "profile.data")
})

P %<>% post_filter_metadata()

test_that("Constructing profile.data using imetadata and ifeatdata works", {
  P0 <- profile.data(imetadata = meta(P), ifeatdata = feat(P))
  expect_equal(P$metadata  %>% dplyr::arrange(xid),
               P0$metadata %>% dplyr::arrange(xid))
  expect_equal(P$featdata  %>% dplyr::arrange(xid),
               P0$featdata %>% dplyr::arrange(xid))
})
