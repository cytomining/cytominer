context("drop_outlier_rows")

test_that("`drop_outlier_rows` works", {
  set.seed(123)

  generate_matrix <- function(n = 30) {
    m <- matrix(rnorm(n * 2), n, 2) %>% scale(.)

    cvec <- matrix(rnorm(2), 1, 2) * 2

    svec <- c(1, 1 + rnorm(1) / 2)

    m <-
      m %>% sweep(., 2, svec, FUN = "*") %>% sweep(., 2, cvec, FUN = "+")

    n_out <- sample(ceiling(n / 20), 1)

    for (k in seq(n_out)) {
      i <- sample(n, 1)

      m[i,] <- m[i,] * 5
    }

    n_na <- sample(ceiling(n / 20), 1)

    for (k in seq(n_na)) {
      i <- sample(n, 1)

      j <- sample(2, 1)

      m[i, j] <- NA

    }

    as.data.frame(m)
  }

  n <- 1000
  data <-
    dplyr::bind_rows(
      generate_matrix(n) %>%
        dplyr::mutate(g1 = "a", g2 = "x"),
      generate_matrix(n) %>%
        dplyr::mutate(g1 = "a", g2 = "y"),
      generate_matrix(n) %>%
        dplyr::mutate(g1 = "b", g2 = "x"),
      generate_matrix(n) %>%
        dplyr::mutate(g1 = "b", g2 = "y")
    )

  data %<>% dplyr::mutate(g3 = seq(nrow(data)))

  data <-
    data %>%
    dplyr::rename(x = V1, y = V2)

  data_cleaned <-
    drop_outlier_rows(
      population = data,
      variables = c("x", "y"),
      strata = c("g1", "g2"),
      sample = data,
      operation = "svd+iqr"
    )

  ggplot2::ggplot(data,
                  ggplot2::aes(x, y, color = interaction(g1, g2))) +
    ggplot2::geom_point() + ggplot2::coord_equal()


  ggplot2::ggplot(data_cleaned,
                  ggplot2::aes(x, y, color = is_outlier)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(g1 ~ g2) +
    ggplot2::coord_equal()

  expect_true(nrow(data_cleaned) > 0)

})
