context("whiten")

test_that("`whiten` whiten data", {
  data <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10), z = rnorm(10))

  db <- DBI::dbConnect(RSQLite::SQLite(),
    ":memory:",
    loadable.extensions = TRUE
  )

  data <- dplyr::copy_to(db, data)

  expect_equal(
    whiten(
      population = data,
      variables = c("x", "y", "z"),
      sample = data,
      regularization_param = 0
    ) %>%
      dplyr::select(c("PC1", "PC2", "PC3")) %>%
      dplyr::collect() %>%
      cov() %>%
      as.matrix() %>%
      unname(),
    diag(rep(1, 3)),
    tolerance = 10^-10
  )

  DBI::dbDisconnect(db)
})
