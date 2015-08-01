context("Measure similarity between vectors")

# Strip metadata column names
names(cpseedseq) <-
  stringr::str_replace(names(cpseedseq),
                       "MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata",
                       "Metadata")


# Create a smaller version of the cpseedseq dataset
cpseedseq_pruned <-
  with(cpseedseq,
       cpseedseq[Metadata_GeneSymbol %in%
                  c("HDAC1", "HDAC2", "HDAC7"),])
hdac1 <-
  with(cpseedseq_pruned,
       cpseedseq_pruned[Metadata_GeneSymbol %in%
                          c("HDAC1"),])
hdac2 <-
  with(cpseedseq_pruned,
       cpseedseq_pruned[Metadata_GeneSymbol %in%
                          c("HDAC2"),])

metadata_cols <- stringr::str_subset(names(cpseedseq), "Metadata_")

cmat <- compute_similarity(hdac1, hdac2,
                           grouping_cols = metadata_cols,
                           melt = F)
cmat_melt <- compute_similarity(hdac1, hdac2,
                                grouping_cols = metadata_cols,
                                melt = T)
test_that("Similarity matrix for test dataset is valid: data.frame", {
  expect_true(
    all(dim(cmat) == c(NROW(hdac1),NROW(hdac2)))
  )
  expect_true(
    max(cmat) <= 1
  )
  expect_true(
    min(cmat) >= -1
  )
  expect_true(
    all(dim(cmat) == c(33, 33))
  )
})


cmp <- list(
  data.frame(Metadata_Well = "n10", Metadata_Plate = 38034),
  data.frame(Metadata_Well = "m24", Metadata_Plate = 38034))

cmp_prf <- list(
  data.frame(Well = "n10", Plate = 38034),
  data.frame(Well = "m24", Plate = 38034))


test_that("format_pair_query gives expected output: data.frame", {
  expect_equivalent(data.frame(Metadata_Well.x = "n10", Metadata_Plate.x = 38034,
                               Metadata_Well.y = "m24", Metadata_Plate.y = 38034),
                    format_pair_query(cmp, names(cmat_melt)))
})


test_that("Melted similarity matrix is valid: data.frame", {
  expect_true(
    all(paste(metadata_cols, "x", sep = ".") %in% names(cmat_melt)),
    info = stringr::str_join(setdiff(paste(metadata_cols, "x", sep = "."),
                                     names(cmat_melt)), collapse = ",")
  )
  expect_true(
    all(paste(metadata_cols, "y", sep = ".") %in% names(cmat_melt)),
    info = stringr::str_join(setdiff(paste(metadata_cols, "y", sep = "."),
                                     names(cmat_melt)), collapse = ",")
  )
  expect_equal(
    dplyr::inner_join(cmat_melt,
                     format_pair_query(cmp, names(cmat_melt)))$value,
    0.4313986,
    tol = 1e-05
  )
})



# Perform similar tests on cpseedseq_prf dataset

cmat_prf <- compute_similarity(cpseedseq_prf,
                               data.frame(GeneSymbol = "HDAC1"),
                               data.frame(GeneSymbol = "HDAC2"),
                               melt = F)
test_that("Similarity matrix for test dataset is valid: profile.data", {
  expect_true(
    max(cmat_prf) <= 1
  )
  expect_true(
    min(cmat_prf) >= -1
  )
  expect_true(
    all(dim(cmat_prf) == c(33, 33))
  )
})

cmat_prf_melt <- compute_similarity(cpseedseq_prf,
                                    data.frame(GeneSymbol = "HDAC1"),
                                    data.frame(GeneSymbol = "HDAC2"),
                                    melt = T)



cmat_melt_s <- cmat_melt
names(cmat_melt_s) <- gsub("Metadata_", "", names(cmat_melt_s))
sel <- c("Plate.x", "Well.x", "Plate.y", "Well.y", "value")





test_that("format_pair_query gives expected output: profile.data", {
  expect_equivalent(data.frame(Well.x = "n10", Plate.x = 38034,
                               Well.y = "m24", Plate.y = 38034),
                    format_pair_query(cmp_prf, names(cmat_prf_melt)))
})


test_that("Melted similarity matrix is valid: profile.data", {
  expect_true(
    all(paste(names(cpseedseq_prf$metadata), "x", sep = ".") %in%
          names(cmat_prf_melt)),
    info = stringr::str_join(setdiff(paste(names(cpseedseq_prf$metadata), "x", sep = "."),
                                     names(cmat_prf_melt)), collapse = ",")
  )
  expect_true(
    all(paste(names(cpseedseq_prf$metadata), "y", sep = ".") %in%
          names(cmat_prf_melt)),
    info = stringr::str_join(setdiff(paste(names(cpseedseq_prf$metadata), "y", sep = "."),
                                     names(cmat_prf_melt)), collapse = ",")
  )

  expect_true(all(
    with(
      merge(cmat_melt_s[, sel], cmat_prf_melt[,sel], setdiff(sel, "value")),
      value.x == value.y
      )
    ))

  expect_equal(
    dplyr::inner_join(cmat_prf_melt,
                      format_pair_query(cmp_prf, names(cmat_prf_melt)))$value,
    0.4313986, tol = 1e-05)
})

# cmat_obj <- compute_similarity(cpseedseq_prf,
#                                data.frame(GeneSymbol = "HDAC1"),
#                                data.frame(GeneSymbol = "HDAC2"),
#                                return_index = T)

test_that("Similarity matrix for test dataset is valid: sim.mat", {

  skip("Skipping because sim.mat is buggy")

  expect_true(
    all(dim(cmat_obj$smat) == c(NROW(hdac1),NROW(hdac2)))
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

  skip("Skipping because sim.mat is buggy")

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

  skip("Skipping because sim.mat is buggy")

  expect_equal(query(cmat_obj,
                     format_pair_query(cmp_prf,
                                       c(paste(names(row_meta(cmat_obj)), "x", sep = "."),
                                         paste(names(col_meta(cmat_obj)), "y", sep = "."))
                                       ))$value,
               0.4313986, tol = 1e-05)

  expect_is(
    query(cmat_obj,
          rbind(data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000197086"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000004815"))),
    "data.frame"
  )

  expect_equal(
    query(cmat_obj,
          rbind(data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000197086"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000004815")))$value,
      c(0.7573881, 0.7337659,
        0.6907641, 0.7675434,
        0.7014778, 0.6333144,
        0.7679370, 0.7347597,
        0.6735144, 0.7573881,
        0.7337659, 0.6907641,
        0.7675434, 0.7014778,
        0.6333144, 0.7679370,
        0.7347597, 0.6735144), tol = 1e-05
  )

  expect_equal(nrow(
    query(cmat_obj,
          rbind(data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000197086"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000004815"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000004815")
                ))),
    18
  )

  expect_equal(
    query(cmat_obj,
          rbind(data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000197086"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "dummyvalue"),
                data.frame(shRNA_CloneID.x = "TRCN0000004815", shRNA_CloneID.y = "TRCN0000004815")))$value,
    c(0.7573881, 0.7337659,
      0.6907641, 0.7675434,
      0.7014778, 0.6333144,
      0.7679370, 0.7347597,
      0.6735144, 0.7573881,
      0.7337659, 0.6907641,
      0.7675434, 0.7014778,
      0.6333144, 0.7679370,
      0.7347597, 0.6735144), tol = 1e-05
  )
})


