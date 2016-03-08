test_that("correlation thresholded intensities is valid", {

  set.seed(123)
  dat <- data.frame(x = rnorm(30))
  dat$y <- rnorm(30)/1000
  dat$z <- dat$x + rnorm(30)/1000

  findCorrelation(cor(dat))

  expect_equal(
    correlation_threshold(population = dat,
                          variables = c('x', 'y', 'z'),
                          sample = dat),
    c("z")
  )

  expect_equal(
    correlation_threshold(population = dat,
                          variables = c('x', 'y'),
                          sample = dat),
    character(0)
  )

})
