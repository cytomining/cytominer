context("Test functions that measure similarity between vectors")


library(dplyr)
cpseedseq_pruned <-
  cpseedseq %>%
  dplyr::filter(MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                  c("HDAC1", "HDAC2", "HDAC7"))
hdac1 <-
  cpseedseq_pruned %>%
  dplyr::filter(MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                  c("HDAC1"))
hdac2 <-
  cpseedseq_pruned %>%
  dplyr::filter(MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_GeneSymbol %in%
                  c("HDAC2"))

metadata_cols <- stringr::str_subset(names(cpseedseq),
                                     "MultipleHairpin_2013_03_07_Analysis_Per_Image.Image_Metadata_")
test_that("Similarity matrix for test dataset is valid", {
  expect_true(
    all(dim(compute_similarity(hdac1, hdac2, grouping_cols = metadata_cols)) == c(33,33))
    )

})
