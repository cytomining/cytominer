context("svd_entropy")

test_that("`svd_entropy` measures singular values entropy", {
  X <- data.frame(diag(c(1, 2, 3)))

  a <- c(1, 4, 9) / 14
  b <- c(1, 4) / 5

  Y <- svd_entropy(c("X1", "X2", "X3"), X, cores = 1)

  expect_equal(
    Y[Y$variable == "X3", ]$svd_entropy,
    -sum(a * log10(a)) + sum(b * log10(b))
  )
})
