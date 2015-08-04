context("Measure similarity within group")

test_that("Within group similarity matrices are valid", {

  cpseedseq_prf_sample <- cpseedseq_prf
  cpseedseq_prf_sample$metadata %<>% dplyr::sample_n(50)
  cpseedseq_prf_sample %<>% post_filter_metadata()
  cmat_l <-  compute_similarity_within_group(cpseedseq_prf_sample,
                                             c("Plate"))

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

test_that("compute_similarity_within_group returns correct values", {

  cpseedseq_prf_sample <- cpseedseq_prf
  cpseedseq_prf_sample$metadata %<>% dplyr::filter(Well %in% c("a01", "a02", "a03"))
  cpseedseq_prf_sample %<>% post_filter_metadata()

  cmat_l <-  compute_similarity_within_group(cpseedseq_prf_sample,
                                             c("Plate"))

  # query should return three rows
  query_res <- query_set(cmat_l,
                         data.frame(Well.x = "a01", Well.y = "a02"),
                         return_all_cols = T )

  futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))

  expect_is(query_res, "data.frame")

  expect_equal(nrow(query_res), 3)

  # query should return empty
  query_res <- query_set(cmat_l,
                         data.frame(Well.x = "a01", Well.y = "dummy"),
                         return_all_cols = T )

  futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))

  expect_is(query_res, "data.frame")

  expect_equal(nrow(query_res), 0)

  # query should return one result
  query_res <- query_set(cmat_l,
                         data.frame(Well.x = "a01", Well.y = "a02", Plate.y = 38003),
                         return_all_cols = T )

  futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))

  expect_is(query_res, "data.frame")

  expect_equal(nrow(query_res), 1)
})
