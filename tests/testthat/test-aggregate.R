test_that("aggregated works with sqlite", {

  dat <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_equal(
    aggregate(population = dat,
              variables = c("x", "y"),
              grouping_variables = c("g")) %>%
      dplyr::collect(),
    dat %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_each_(dplyr::funs(mean), vars = c("x", "y"))
  )

})
