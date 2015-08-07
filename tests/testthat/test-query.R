context("Query similarity matrix")

test_that("Querying based on equality of columns - small sim.mat", {
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

  # query should return 27 rows
  query_res <- query(cmat_l[[1]],
                     equality_join_cols = c("Plate"),
                     return_all_cols = T )

  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), 3 * 3 * 3)
  #futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res))

  # query should return 9 rows
  query_res <- query(cmat_l[[1]],
                     equality_join_cols = c("Plate", "Well"),
                     return_all_cols = T )

  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), 3 * 3)
  #futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res %>% dplyr::select(Plate, Well)))

})

test_that("Querying based on equality of columns - large sim.mat", {

  cpseedseq_prf$metadata %<>% dplyr::mutate(data_id =
                                            digest::digest(cpseedseq_prf))

  cmat_l <-  compute_similarity_within_group(cpseedseq_prf,
                                             c("data_id"))

  testthat::expect_equal(length(cmat_l), 1)

  # query should return 384 * 3 rows
  query_res <- query(cmat_l[[1]],
                     equality_join_cols = c("Plate", "Well"),
                     return_all_cols = T )

  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), 384 * 3)
  #futile.logger::flog.debug("Result = %s", jsonlite::toJSON(query_res %>% dplyr::select(Plate, Well)))

  # query should return 99 rows
  cpseedseq_prf_sample <- cpseedseq_prf
  cpseedseq_prf_sample$metadata %<>% dplyr::filter(GeneSymbol == "HDAC1")
  cpseedseq_prf_sample %<>% post_filter_metadata()
  cpseedseq_prf_sample$metadata %<>%
    dplyr::mutate(data_id = digest::digest(cpseedseq_prf_sample))

  cmat_l <- compute_similarity_within_group(cpseedseq_prf_sample,
              c("data_id"))
  query_res <- query(cmat_l[[1]],
                     equality_join_cols = c("shRNA_CloneID"),
                     return_all_cols = T )

  expect_is(query_res, "data.frame")
  expect_equal(nrow(query_res), (11*3*3))

})

# query should return 198 rows
cpseedseq_prf_sample <- cpseedseq_prf
cpseedseq_prf_sample$metadata %<>% dplyr::filter(GeneSymbol %in% c("HDAC1", "HDAC2"))
cpseedseq_prf_sample %<>% post_filter_metadata()
cpseedseq_prf_sample$metadata %<>%
  dplyr::mutate(data_id = digest::digest(cpseedseq_prf_sample))

cmat_l <- compute_similarity_within_group(cpseedseq_prf_sample,
            c("data_id"))

test_that("Querying based on equality of columns and query_frame", {
  cpseedseq_prf$metadata %<>% dplyr::mutate(data_id =
                                            digest::digest(cpseedseq_prf))

  expect_equal(length(cmat_l), 1)
  query_res <- query(S = cmat_l[[1]],
                     equality_join_cols = c("Plate"),
                     query_frame = data.frame(GeneSymbol.x = "HDAC1",
                                              GeneSymbol.y = "HDAC2",
                                              stringsAsFactors = F),
                     return_all_cols = T)
  expect_is(query_res, "data.frame")

  expect_equal(nrow(query_res), 3*(11**2))
  expect_equal(query_res %>% dplyr::filter(Plate.x != Plate.y) %>% nrow(), 0)
  expect_equal(query_res %>% dplyr::filter(GeneSymbol.x != "HDAC1") %>% nrow(), 0)
  expect_equal(query_res %>% dplyr::filter(GeneSymbol.y != "HDAC2") %>% nrow(), 0)

})

test_that("Renaming return value works as expected", {
  query_res <- query(S = cmat_l[[1]],
                     equality_join_cols = c("Plate"),
                     query_frame = data.frame(GeneSymbol.x = "HDAC1",
                                              GeneSymbol.y = "HDAC2",
                                              stringsAsFactors = F),
                     return_all_cols = F,
                     rename_value_to_metric = T)
  metric_name <- format(metric(cmat_l[[1]]))
  expect_true(metric_name %in% names(query_res),
              info = stringr::str_c(c(names(query_res),
                                    metric_name), collapse = ","))
})
