context("normalize")

test_that("`normalize' normalizes data", {
  set.seed(123)

  generate_matrix <- function(cvec, svec) {
    n <- 30

    m <- matrix(runif(n * 2), n, 2) %>% scale(.)

    m %<>% sweep(., 2, svec, FUN = "*") %>% sweep(., 2, cvec, FUN = "+")

    m[1, 1] <- NA

    cbind(scale(m), m) %>% as.data.frame()
  }

  data <-
    dplyr::bind_rows(
      generate_matrix(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "a", g2 = "x"),
      generate_matrix(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "a", g2 = "y"),
      generate_matrix(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "b", g2 = "x"),
      generate_matrix(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "b", g2 = "y")
    )

  data %<>% dplyr::mutate(g3 = seq(nrow(data)))

  data_normalized <-
    data %>%
    dplyr::select(g1, g2, g3, V1, V2) %>%
    dplyr::rename(x = V1, y = V2)

  data <-
    data %>%
    dplyr::select(g1, g2, g3, V3, V4) %>%
    dplyr::rename(x = V3, y = V4)

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # https://github.com/tidyverse/dplyr/issues/3093
  RSQLite::initExtension(db)

  data <- dplyr::copy_to(db, data)

  expect_lt(
    mean(abs(
      normalize(population = data,
                variables = c("x", "y"),
                strata = c("g1", "g2"),
                sample = data,
                operation = "standardize") %>%
        dplyr::collect() %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix() -
      data_normalized %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix()),
      na.rm = TRUE
      ),
    .Machine$double.eps * 1000000
  )

  #test after collecting so that data.frame -specific scale function is tested
  expect_lt(
    mean(abs(
      normalize(population = data %>% dplyr::collect(),
                variables = c("x", "y"),
                strata = c("g1", "g2"),
                sample = data,
                operation = "standardize") %>%
        dplyr::collect() %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix() -
        data_normalized %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix()),
      na.rm = TRUE
    ),
    .Machine$double.eps * 1000000
  )

  DBI::dbDisconnect(db)
})
