context("drop_na_columns")

test_that("`drop_na_columns` removes columns have only NAs", {

  data <- data.frame(x = rnorm(5), y = NA)

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  data <- dplyr::copy_to(db, data)

  expect_equal(
    drop_na_columns(population = data,
                    variables = c("x", "y")),
    c("y")
  )

  expect_equal(
    drop_na_columns(population = data,
                    variables = c("x")),
    character(0)
  )

  DBI::dbDisconnect(db)
})
