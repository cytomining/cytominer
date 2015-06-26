context("Test functions that measure similarity between vectors")

x <- seq(10)
y <- c(seq(3), seq(7))

test_that("Similarity values are valid", {
  expect_true(compute_similarity(x, y) <= 1)
})
