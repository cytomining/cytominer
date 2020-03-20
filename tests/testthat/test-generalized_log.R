context("generalized_log")

test_that("`generalized_log` generalized_logs data", {
  data <- data.frame(x = rnorm(5), y = rnorm(5))

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  data <- dplyr::copy_to(db, data)

  glog <- function(x, c = 1) log((x + (x^2 + c^2)^0.5) / 2)

  expect_equal(
    generalized_log(
      population = data,
      variables = c("x", "y")
    ) %>% dplyr::collect(),
    glog(data %>% dplyr::collect())
  )

  expect_equal(
    generalized_log(
      population = data,
      variables = c("x")
    ) %>% dplyr::collect(),
    within(data %>% dplyr::collect(), x <- glog(x))
  )

  DBI::dbDisconnect(db)
})
