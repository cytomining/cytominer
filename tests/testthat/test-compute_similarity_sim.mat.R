context("Measure similarity between vectors using sim.mat")

cmp_prf <- list(
  data.frame(Well = "n10", Plate = 38034),
  data.frame(Well = "m24", Plate = 38034))

# this returns a sim.mat object
cmat_obj <- compute_similarity(cpseedseq_prf,
                               data.frame(GeneSymbol = "HDAC1"),
                               data.frame(GeneSymbol = "HDAC2"),
                               return_index = T)

test_that("Similarity matrix for test dataset is valid: sim.mat", {

  expect_true(
    all(dim(cmat_obj$smat) ==
          c(NROW(meta(cpseedseq_prf) %>% dplyr::filter(GeneSymbol == "HDAC1")),
            NROW(meta(cpseedseq_prf) %>% dplyr::filter(GeneSymbol == "HDAC2")))
    )
  )

  expect_true(
    max(cmat_obj$smat) <= 1
  )
  expect_true(
    min(cmat_obj$smat) >= -1
  )
  expect_true(
    all(dim(cmat_obj$smat) == c(33, 33))
  )
})

test_that("sim.mat similarity matrix is valid: profile.data", {

  expect_true(
    all(names(row_meta(cmat_obj)) %in% names(cpseedseq_prf$metadata)),
    info = stringr::str_c(c(names(row_meta(cmat_obj)),
                            names(cpseedseq_prf$metadata)),
                          collapse = ",")
  )

  expect_true(
    all(names(col_meta(cmat_obj)) %in% names(cpseedseq_prf$metadata)),
    info = stringr::str_c(c(names(col_meta(cmat_obj)),
                            names(cpseedseq_prf$metadata)),
                          collapse = ",")
  )
})

test_that("sim.mat similarity matrix returns correct values", {

  expect_equal(query(cmat_obj,
                     format_pair_query(cmp_prf,
                                       c(paste(names(row_meta(cmat_obj)), "x", sep = "."),
                                         paste(names(col_meta(cmat_obj)), "y", sep = "."))
                     ))$value,
               0.4313986, tol = 1e-05)

  expect_is(
    query(cmat_obj,
          data.frame(shRNA_CloneID.x = "TRCN0000004815",
                     shRNA_CloneID.y = "TRCN0000197086")),
    "data.frame"
  )

  expect_equal(
    query(cmat_obj,
          data.frame(shRNA_CloneID.x = "TRCN0000004815",
                     shRNA_CloneID.y = "TRCN0000197086"))$value,
    c(0.7573881, 0.7337659,
      0.6907641, 0.7675434,
      0.7014778, 0.6333144,
      0.7679370, 0.7347597,
      0.6735144),
    tol = 1e-05
  )

  expect_equal(nrow(
    query(cmat_obj,
          data.frame(shRNA_CloneID.x = "TRCN0000004815",
                     shRNA_CloneID.y = "dummy"))),
    0
  )

  expect_equal(nrow(
    query(cmat_obj,
          data.frame(Plate.x = 37983,
                     Plate.y = 38003))),
    11*11
  )

})

cmat_obj_large <- compute_similarity(cpseedseq_prf,
                                     data.frame(Plate = 37983),
                                     data.frame(Plate = 38003),
                                     return_index = T)

cmat_prf_melt_large <- compute_similarity(cpseedseq_prf,
                                    data.frame(Plate = 37983),
                                    data.frame(Plate = 38003),
                                    melt = T)

cmat_prf_melt_large_sample <-
  cmat_prf_melt_large %>%
  dplyr::select(Plate.x, Well.x, Plate.y, Well.y, value) %>%
  dplyr::sample_n(500)

cmat_prf_melt_large_sample_query <-
  cmat_prf_melt_large_sample %>% dplyr::select(-value)

test_that("sim.mat similarity matrix returns correct values - large sim.mat", {
  skip("Skipping because it is too slow")
  expect_equal(nrow(
    query(cmat_obj_large,
          data.frame(Plate.x = 37983,
                     Plate.y = 38003))),
    384*384
  )
})

test_that("sim.mat similarity matrix returns correct values - compare with a reference", {

  expect_equal(
    query_n(cmat_obj_large, cmat_prf_melt_large_sample_query) %>%
      dplyr::arrange(Plate.x, Well.x, Plate.y, Well.y),
    cmat_prf_melt_large_sample %>%
      dplyr::arrange(Plate.x, Well.x, Plate.y, Well.y))

})
