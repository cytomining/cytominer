context("Test replace_metadata")

metadata <-
rbind(
  data.frame(shRNA_CloneID = "TRCN0000329696", ASSAY_WELL_ROLE = "Treated", stringsAsFactors = F),
  data.frame(shRNA_CloneID = "TRCN0000072186", ASSAY_WELL_ROLE = "Control", stringsAsFactors = F)
)


test_that("Replacing new metadata works as expected", {
  cpseedseq_prf_1 <- replace_metadata(cpseedseq_prf,
                                      metadata,
                                      key_cols = "shRNA_CloneID")
  expect_is(replace_metadata(cpseedseq_prf,
                             metadata,
                             key_cols = "shRNA_CloneID"), "profile.data")

  expect_true(all(names(metadata) %in% names(meta(cpseedseq_prf_1))))
  expect_equal(nrow(meta(cpseedseq_prf)), nrow(meta(cpseedseq_prf_1)))
})
