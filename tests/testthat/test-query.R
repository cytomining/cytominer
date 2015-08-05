context("Query similarity matrix")

test_that("Querying based on equality of columns", {
  cpseedseq_prf_sample <- cpseedseq_prf
  cpseedseq_prf_sample$metadata %<>% dplyr::filter(Well %in% c("a01", "a02", "a03"))
  cpseedseq_prf_sample %<>% post_filter_metadata()
  cpseedseq_prf_sample$metadata %<>% dplyr::mutate(data_id = digest::digest(cpseedseq_prf_sample))


  cmat_l <-  compute_similarity_within_group(cpseedseq_prf_sample,
                                             c("data_id"))

  testthat::expect_equal(length(cmat_l), 1)

  # query should return 9 rows
  query_res <- query(cmat_l[[1]],
                     data.frame(Well.x = "a01", Well.y = "a02"),
                     return_all_cols = T )

  #futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))
  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), 9)

  # query should return 3 rows
  query_res <- query(cmat_l[[1]],
                     equality_join_cols = c("Plate"),
                     return_all_cols = T )

  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), 3 * 3 * 3)
  #futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))

})
