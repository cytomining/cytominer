context("Measure similarity between vectors")


# Create a smaller version of the cpseedseq dataset
cpseedseq_pruned <-
  with(cpseedseq,
       cpseedseq[MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                  c("HDAC1", "HDAC2", "HDAC7"),])
hdac1 <-
  with(cpseedseq_pruned,
       cpseedseq_pruned[MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                          c("HDAC1"),])
hdac2 <-
  with(cpseedseq_pruned,
       cpseedseq_pruned[MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                          c("HDAC2"),])

metadata_cols <- stringr::str_subset(names(cpseedseq),
                                     "MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_")
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
})
