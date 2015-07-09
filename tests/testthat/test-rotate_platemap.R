context("Test rotate_platemap")

# Load cpseedseq dataset as a profile.data object
cfg_fname <-
  system.file("extdata",
              "well-summary-profile_mean-median-robust_std-untreated_norm.yml",
              package = "pertminr")
cpseedseq_prf <- profile.data(cfg_fname)

cpseedseq_prf <- process_metadata(cpseedseq_prf,
                                  strip_cellprofiler_db_tags = T)

test_that("Rotating a platemap works as expected", {
  expect_is(rotate_platemap(cpseedseq_prf), "profile.data")
})
