context("correlation_threshold")

test_that(
  "`correlation_threshold` selects variables that are not highly correlated", {

  set.seed(123)
  data <- data.frame(x = rnorm(30))
  data$y <- rnorm(30) / 1000
  data$z <- data$x + rnorm(30) / 1000

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  data <- dplyr::copy_to(db, data)

  expect_equal(
    correlation_threshold(variables = c("x", "y", "z"),
                          sample = data %>% dplyr::collect()),
    c("z")
  )

  expect_equal(
    correlation_threshold(variables = c("x", "y"),
                          sample = data %>% dplyr::collect()),
    character(0)
  )

  DBI::dbDisconnect(db)
})
