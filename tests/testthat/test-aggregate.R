context("aggregate")

test_that("`aggregate` aggregates data", {

  data <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  data <- dplyr::copy_to(db, data)

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g")) %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_each_(dplyr::funs(mean), vars = c("x", "y"))
  )

  DBI::dbDisconnect(db)
})
