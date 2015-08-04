context("Test add_featdata")

test_that("Adding new featdata works as expected", {

  expect_is(add_featdata(cpseedseq_prf, cpseedseq_cnt_prf), "profile.data")

  cpseedseq_prf_1 <- add_featdata(cpseedseq_prf, cpseedseq_cnt_prf)

  expect_true(all(names(feat(cpseedseq_cnt_prf)) %in%  names(feat(cpseedseq_prf_1))))
  expect_equal(nrow(feat(cpseedseq_prf)), nrow(feat(cpseedseq_prf_1)))
  expect_equal(
    full(cpseedseq_prf_1) %>%
      dplyr::select_(.dots = names(full(cpseedseq_cnt_prf))) %>%
      dplyr::arrange_(.dots = names(meta(cpseedseq_cnt_prf))),
    full(cpseedseq_cnt_prf) %>%
      dplyr::arrange_(.dots = names(meta(cpseedseq_cnt_prf)))
  )
})
