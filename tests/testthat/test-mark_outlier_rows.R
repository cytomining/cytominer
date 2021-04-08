context("mark_outlier_rows")

test_that("`mark_outlier_rows` works", {
  set.seed(123)

  generate_matrix <- function(n = 30, n_out = 5) {
    m <- matrix(rnorm(n * 2), n, 2) %>% scale(.)

    cvec <- matrix(rnorm(2), 1, 2) * 2

    svec <- c(1, 1 + rnorm(1) / 2)

    m <-
      m %>%
      sweep(., 2, svec, FUN = "*") %>%
      sweep(., 2, cvec, FUN = "+")

    for (k in seq(n_out)) {
      i <- sample(n, 1)

      m[i, ] <- m[i, ] * 5
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
  n_out <- 50

  data <-
    dplyr::bind_rows(
      generate_matrix(n, n_out) %>%
        dplyr::mutate(g1 = "a", g2 = "x"),
      generate_matrix(n, n_out) %>%
        dplyr::mutate(g1 = "a", g2 = "y"),
      generate_matrix(n, n_out) %>%
        dplyr::mutate(g1 = "b", g2 = "x"),
      generate_matrix(n, n_out) %>%
        dplyr::mutate(g1 = "b", g2 = "y")
    )

  data %<>% dplyr::mutate(g3 = seq(nrow(data)))

  data <-
    data %>%
    dplyr::rename(x = V1, y = V2)

  data_cleaned <-
    stratify(
      reducer = cytominer::mark_outlier_rows,
      population = data,
      variables = c("x", "y"),
      strata = c("g1", "g2"),
      sample = data,
      method = "svd+iqr"
    )

  data_cleaned_no_strata <-
    mark_outlier_rows(
      population = data,
      variables = c("x", "y"),
      sample = data,
      operation = "svd+iqr"
    )

  expect_true(
    data_cleaned_no_strata %>%
      dplyr::group_by(is_outlier) %>%
      dplyr::tally(name = "n_outliers") %>%
      dplyr::filter(is_outlier) %>%
      dplyr::mutate(check = n_outliers > n_out * 4 * .75) %>%
      dplyr::pull(check)
  )

  # ggplot2::ggplot(
  #   na.omit(data_cleaned),
  #   ggplot2::aes(x, y, color = is_outlier)
  # ) +
  #   ggplot2::geom_point() +
  #   ggplot2::facet_grid(g1 ~ g2) +
  #   ggplot2::coord_equal()
  #
  # ggplot2::ggplot(
  #   na.omit(data_cleaned_no_strata),
  #   ggplot2::aes(x, y, color = is_outlier)
  # ) +
  #   ggplot2::geom_point() +
  #   ggplot2::coord_equal()

  expect_true(
    data_cleaned %>%
      dplyr::group_by(g1, g2, is_outlier) %>%
      dplyr::tally(name = "n_outliers") %>%
      dplyr::filter(is_outlier) %>%
      dplyr::mutate(check = n_outliers > n_out) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(check = all(check)) %>%
      dplyr::pull(check)
  )

  expect_true(
    data_cleaned %>%
      dplyr::group_by(is_outlier) %>%
      dplyr::tally(name = "n_outliers") %>%
      dplyr::filter(is_outlier) %>%
      dplyr::mutate(check = n_outliers > n_out * 4) %>%
      dplyr::pull(check)
  )

  expect_true(
    dplyr::all_equal(
      data_cleaned %>% stats::na.omit() %>% dplyr::select(-is_outlier) %>% as.data.frame(),
      data %>% stats::na.omit() %>% as.data.frame()
    )
  )
})
