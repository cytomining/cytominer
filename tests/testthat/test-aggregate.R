context("aggregate")

test_that("`aggregate` aggregates data", {

  data <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  #https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  data <- dplyr::copy_to(db, data)

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = dplyr::funs(median = median),
                          .vars = c("x", "y"))
  )

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = dplyr::funs(median = median),
                          .vars = c("x", "y"))
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

  DBI::dbDisconnect(db)

})
