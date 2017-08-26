context("generalized_log")

test_that("`generalized_log` generalized_logs data", {

  data <- data.frame(x = rnorm(5), y = rnorm(5))

  # The call to dplyr::src_sqlite was not changed to DBI::dbConnect
  # because it results in an error "no such function: log"
  # db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # data <- dplyr::copy_to(db, data)
  data <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        data)

  glog <- function(x, c=1) log( (x + ( x ^ 2 + c ^ 2) ^ 0.5 ) / 2 )

  expect_equal(
    generalized_log(population = data,
                    variables = c("x", "y")) %>% dplyr::collect(),
    glog(data %>% dplyr::collect())
  )

  expect_equal(
    generalized_log(population = data,
                    variables = c("x")) %>% dplyr::collect(),
    within(data %>% dplyr::collect(), x <- glog(x))
  )
})
