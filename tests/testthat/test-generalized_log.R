context("generalized_log")

test_that("`generalized_log` generalized_logs data", {

  dat <- data.frame(x = rnorm(5), y = rnorm(5))

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  glog <- function(x, c=1) log( (x + ( x ^ 2 + c ^ 2) ^ 0.5 ) / 2 )

  expect_equal(
    generalized_log(population = dat,
                    variables = c("x", "y")) %>% dplyr::collect(),
    glog(dat %>% dplyr::collect())
  )

  expect_equal(
    generalized_log(population = dat,
                    variables = c("x")) %>% dplyr::collect(),
    within(dat %>% dplyr::collect(), x <- glog(x))
  )
})
