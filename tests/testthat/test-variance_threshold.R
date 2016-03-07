test_that("variance threshold is valid", {

  dat <- data.frame(x = rnorm(30), y = 1)

  expect_equal(
    variance_threshold(population = dat,
                       variables = c('x', 'y'),
                       sample = dat),
    c("y")
  )

  expect_equal(
    variance_threshold(population = dat,
                       variables = c('x'),
                       sample = dat),
    character(0)
  )

})
