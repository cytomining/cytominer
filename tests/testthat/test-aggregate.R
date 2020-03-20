context("aggregate")

test_that("`aggregate` aggregates data", {
  data <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  data <- dplyr::copy_to(db, data)

  expect_equal(
    aggregate(
      population = data,
      variables = c("x", "y"),
      strata = c("g"),
      operation = "median"
    ) %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(
        .funs = ~ MEDIAN(.),
        .vars = c("x", "y")
      )
  )

  expect_equal(
    aggregate(
      population = data,
      variables = c("x", "y"),
      strata = c("g"),
      operation = "median"
    ) %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(
        .funs = ~ MEDIAN(.),
        .vars = c("x", "y")
      )
  )

  expect_equal(
    aggregate(
      population = data,
      variables = c("x", "y"),
      strata = c("g"),
      operation = "mean+sd"
    ) %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(
        .funs = c(~ mean(., na.rm = T), ~ sd(., na.rm = T)),
        .vars = c("x", "y")
      )
  )

  cov_a <-
    data %>%
    dplyr::filter(g == "a") %>%
    dplyr::select(x, y) %>%
    dplyr::collect() %>%
    stats::cov()

  expect_equal(
    aggregate(
      population = data,
      variables = c("x", "y"),
      strata = c("g"),
      operation = "covariance",
      univariate = FALSE
    ) %>%
      dplyr::filter(g == "a") %>%
      dplyr::select(-g),
    tibble::tibble(
      x__x = cov_a[1, 1],
      y__x = cov_a[1, 2],
      y__y = cov_a[2, 2]
    )
  )

  DBI::dbDisconnect(db)
})
