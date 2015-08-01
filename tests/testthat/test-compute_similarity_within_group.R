context("Measure similarity within group")

cpseedseq_prf_sample <- cpseedseq_prf
cpseedseq_prf_sample$metadata %<>% dplyr::sample_n(50)

cmat_l <-  compute_similarity_within_group(cpseedseq_prf_sample,
                                           c("Plate"))

test_that("Within group similarity matrices are valid", {

  expect_is(cmat_l, "list")
  expect_is(cmat_l[[1]], "sim.mat")
  expect_is(cmat_l[[2]], "sim.mat")
  expect_is(cmat_l[[3]], "sim.mat")

  expect_true(max(smat(cmat_l[[1]])) <= 1)
  expect_true(min(smat(cmat_l[[1]])) >= -1)
  expect_true(max(smat(cmat_l[[2]])) <= 1)
  expect_true(min(smat(cmat_l[[2]])) >= -1)
  expect_true(max(smat(cmat_l[[3]])) <= 1)
  expect_true(min(smat(cmat_l[[3]])) >= -1)

})
