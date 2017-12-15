context("entropy_feature_selection")

test_that("`entropy_feature_selection` selects features based on the singular values entropy", {

  set.seed(24)
  xa <- rnorm(5)
  xb <- rnorm(5)
  ya <- rnorm(5)
  yb <- rnorm(5)

  data <-
    rbind(
      data.frame(g = "a", x = xa, y = ya, z = xa + ya),
      data.frame(g = "b", x = xb, y = yb, z = xb + yb)
    )

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  RSQLite::initExtension(db)

  data <- dplyr::copy_to(db, data)

  expect_equal(
    sort(entropy_feature_selection(population = data, variables = c("x", "y", "z"), n_feature = 2)[["features"]],
         decreasing = F),
    c("x", "y")
  )

  DBI::dbDisconnect(db)

})
