context("drop_na_columns")

test_that("`drop_na_columns` removes columns have only NAs", {

  dat <- data.frame(x = rnorm(5), y = NA)

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    drop_na_columns(population = dat,
                    variables = c("x", "y")),
    c("y")
  )

  expect_equal(
    drop_na_columns(population = dat,
                    variables = c("x")),
    character(0)
  )

})
