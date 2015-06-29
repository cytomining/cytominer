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
cmat <- compute_similarity(hdac1, hdac2, grouping_cols = metadata_cols)

test_that("Similarity matrix for test dataset is valid", {
  expect_true(
    all(dim(cmat) == c(NROW(hdac1),NROW(hdac2)))
    )

})
