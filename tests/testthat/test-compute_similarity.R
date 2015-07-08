context("Measure similarity between vectors")

# Strip metadata column names
names(cpseedseq) <- stringr::str_replace(names(cpseedseq),
                                         "MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata", "Metadata")


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
cmat <- compute_similarity(hdac1, hdac2, grouping_cols = metadata_cols, melt = F)
cmat_melt <- compute_similarity(hdac1, hdac2, grouping_cols = metadata_cols, melt = T)

test_that("Similarity matrix for test dataset is valid", {
  expect_true(
    all(dim(cmat) == c(NROW(hdac1),NROW(hdac2)))
    )
  expect_true(
    max(cmat) <= 1
  )
  expect_true(
    min(cmat) >= -1
  )

})

cmp <- list(
  data.frame(Metadata_Well = "n10", Metadata_Plate = 38034),
  data.frame(Metadata_Well = "m24", Metadata_Plate = 38034))


test_that("format_pair_query gives expected output", {
  expect_equivalent(data.frame(Metadata_Well.x = "n10", Metadata_Plate.x = 38034,
                               Metadata_Well.y = "m24", Metadata_Plate.y = 38034),
               format_pair_query(cmp, names(cmat_melt)))
})


test_that("Melted similarity matrix is valid", {
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



