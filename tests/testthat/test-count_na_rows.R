context("count_na_rows")

test_that("`count_na_rows` returns the frequency of NAs per variable", {
  data <- data.frame(x = rnorm(5), y = rnorm(5), z = rnorm(5))
  data[c(1, 2), "x"] <- NA
  data[c(2, 3, 5), "y"] <- NA

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  data <- dplyr::copy_to(db, data)

  expect_equal(
    count_na_rows(
      population = data,
      variables = c("x", "y", "z")
    ),
    data.frame(x = 2, y = 3, z = 0)
  )

  DBI::dbDisconnect(db)
})
