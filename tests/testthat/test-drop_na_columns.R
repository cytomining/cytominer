test_that("Remove NA works with sqlite", {

  dat <- data.frame(x = rnorm(5), y = NA)

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    drop_na_columns(population = dat,
                    variables = c('x', 'y')),
    c("y")
  )

  expect_equal(
    drop_na_columns(population = dat,
                    variables = c('x'),
                    sample = dat),
    character(0)
  )

})
