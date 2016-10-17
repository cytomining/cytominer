context("replicate_correlation")

test_that("`replicate_correlation` measure correlation between replicates in each feature", {
  
  set.seed(123)
  
  x1 <- rnorm(10)
  x2 <- x1 + rnorm(10) / 100
  y1 <- rnorm(10)
  y2 <- y1 + rnorm(10) / 10
  z1 <- rnorm(10)
  z2 <- z1 + rnorm(10) / 1
  
  correlations <- 
    tibble::data_frame(
      variable = c('x', 'y', 'z'),
      median = c(
        cor(x1, x2, method = "pearson"),
        cor(y1, y2, method = "pearson"),
        cor(z1, z2, method = "pearson")
        ))

  batch <- rep(rep(1:2, each=5), 2)
  
  cpd <- rep(1:10, 2)
  
  replicate_id <- rep(1:2, each=10)
  
  data <- data.frame(x = c(x1, x2),
                     y = c(y1, y2),
                     z = c(z1, z2),
                     cpd,
                     replicate_id,
                     batch
                     )
  expect_equal(
    replicate_correlation(sample = data,
                          variables = c("x", "y", "z"),
                          strata = c("cpd"),
                          replicates = 2,
                          cores = 2) %>%
      dplyr::select_(.dots = c('variable', 'median')) %>%
      dplyr::arrange_(.dots = c('variable')),
    correlations
  )
  
  expect_equal(
    replicate_correlation(sample = data,
                          variables = c("x", "y", "z"),
                          strata = c("cpd"),
                          replicates = 2,
                          replicate_by = "replicate_id",
                          cores = 2) %>%
      dplyr::select_(.dots = c('variable', 'median')) %>%
      dplyr::arrange_(.dots = c('variable')),
    correlations
  )
  
})
