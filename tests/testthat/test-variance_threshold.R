context("variance threshold")

test_that("variance threshold works with sqlite", {

  dat <- data.frame(x = rnorm(30), y = 1)

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    variance_threshold(population = dat,
                       variables = c('x', 'y'),
                       sample = dat %>% dplyr::collect()),
    c("y")
  )

  expect_equal(
    variance_threshold(population = dat,
                       variables = c('x'),
                       sample = dat %>% dplyr::collect()),
    character(0)
  )

})
