context("aggregate")

test_that("`aggregate` aggregates data", {

  data <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  db <- dplyr::src_sqlite(":memory:", create = T)

  data <- dplyr::copy_to(db, data)

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = dplyr::funs(median), .vars = c("x", "y"))
  )

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = dplyr::funs(median), .vars = c("x", "y"))
  )

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "mean+sd") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = c(dplyr::funs(mean), dplyr::funs(sd)),
                          .vars = c("x", "y"))
  )
})
