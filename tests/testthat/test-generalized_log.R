test_that("generalized log of intensity is valid", {

  dat <- data.frame(x = rnorm(5), y = rnorm(5))

  glog <- function(x, c=1) log( (x + ( x ^ 2 + c ^ 2) ^ 0.5 ) / 2 )

  expect_equal(
    generalized_log(population = dat,
                    variables = c('x', 'y')),
    glog(dat)
  )

  expect_equal(
    generalized_log(population = dat,
                    variables = c('x')),
    within(dat, x <- glog(x))
  )
})
