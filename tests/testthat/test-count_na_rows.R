context("count_na_rows")

test_that("`count_na_rows` returns the frequency of NAs per variable", {

  dat <- data.frame(x = rnorm(5), y = rnorm(5), z = rnorm(5))
  dat[c(1, 2), "x"] <- NA
  dat[c(2, 3, 5), "y"] <- NA

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    count_na_rows(population = dat,
                    variables = c("x", "y", "z")),
    data.frame(x = 2, y = 3, z = 0)
  )

})
