context("variance_threshold")

test_that(
  "`variance_threshold` selects variables that have non-trivial variance", {

  data <- data.frame(x = rnorm(30), y = 1)

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  data <- dplyr::copy_to(db, data)

  expect_equal(
    variance_threshold(variables = c("x", "y"),
                       sample = data %>% dplyr::collect()),
    c("y")
  )

  expect_equal(
    variance_threshold(variables = c("x"),
                       sample = data %>% dplyr::collect()),
    character(0)
  )

  DBI::dbDisconnect(db)
})
