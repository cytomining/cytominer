context("correlation threshold")

test_that("correlation threshold works with sqlite", {

  set.seed(123)
  dat <- data.frame(x = rnorm(30))
  dat$y <- rnorm(30)/1000
  dat$z <- dat$x + rnorm(30)/1000

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    correlation_threshold(population = dat,
                          variables = c('x', 'y', 'z'),
                          sample = dat %>% dplyr::collect()),
    c("z")
  )

  expect_equal(
    correlation_threshold(population = dat,
                          variables = c('x', 'y'),
                          sample = dat %>% dplyr::collect()),
    character(0)
  )

})
