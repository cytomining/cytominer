context("replicate_correlation")

test_that(paste0(
  "`replicate_correlation` measures correlation",
  "between replicates in each feature"
), {
  set.seed(123)

  x1 <- rnorm(10)
  x2 <- x1 + rnorm(10) / 100
  y1 <- rnorm(10)
  y2 <- y1 + rnorm(10) / 10
  z1 <- rnorm(10)
  z2 <- z1 + rnorm(10) / 1

  correlations <-
    tibble::tibble(
      variable = c("x", "y", "z"),
      median = c(
        cor(x1, x2, method = "pearson"),
        cor(y1, y2, method = "pearson"),
        cor(z1, z2, method = "pearson")
      )
    )

  correlations_batched <-
    tibble::tibble(
      variable = c("x", "y", "z"),
      b1 = c(
        cor(x1[1:5], x2[1:5], method = "pearson"),
        cor(y1[1:5], y2[1:5], method = "pearson"),
        cor(z1[1:5], z2[1:5], method = "pearson")
      ),
      b2 = c(
        cor(x1[6:10], x2[6:10], method = "pearson"),
        cor(y1[6:10], y2[6:10], method = "pearson"),
        cor(z1[6:10], z2[6:10], method = "pearson")
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      median = median(c(b1, b2)),
      min = min(b1, b2),
      max = max(b1, b2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-b1, -b2)

  batch <- rep(rep(1:2, each = 5), 2)

  cpd <- rep(1:10, 2)

  replicate_id <- rep(1:2, each = 10)

  data <- data.frame(
    x = c(x1, x2),
    y = c(y1, y2),
    z = c(z1, z2),
    cpd,
    replicate_id,
    batch
  )
  expect_equal(
    replicate_correlation(
      sample = data,
      variables = c("x", "y", "z"),
      strata = c("cpd"),
      replicates = 2,
      cores = 2
    ) %>%
      dplyr::select(variable, median) %>%
      dplyr::arrange(variable) %>%
      as.data.frame(),
    correlations %>%
      as.data.frame(),
    tolerance = 10e-12
  )

  expect_equal(
    replicate_correlation(
      sample = data,
      variables = c("x", "y", "z"),
      strata = c("cpd"),
      replicates = 2,
      replicate_by = "replicate_id",
      cores = 2
    ) %>%
      dplyr::select(variable, median) %>%
      dplyr::arrange(variable) %>%
      as.data.frame(),
    correlations %>%
      as.data.frame(),
    tolerance = 10e-12
  )

  expect_equal(
    replicate_correlation(
      sample = data,
      variables = c("x", "y", "z"),
      strata = c("cpd"),
      replicates = 2,
      split_by = "batch",
      replicate_by = "replicate_id",
      cores = 2
    ) %>%
      dplyr::arrange(variable) %>%
      as.data.frame(),
    correlations_batched %>%
      as.data.frame(),
    tolerance = 10e-12
  )
})
