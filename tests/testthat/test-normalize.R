context("normalize")

test_that("`normalize' normalizes data", {
  set.seed(123)
  generate_mat <- function(cvec, svec) {
    n <- 30

    m <- matrix(runif(n * 2), n, 2) %>% scale(.)

    cbind(m, m %>% sweep(., 2, svec, FUN = "*") %>% sweep(., 2, cvec, FUN = "+")) %>% as.data.frame()
  }

  dat <-
    dplyr::bind_rows(
      generate_mat(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "a", g2 = "x"),
      generate_mat(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "a", g2 = "y"),
      generate_mat(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "b", g2 = "x"),
      generate_mat(rnorm(2), rnorm(2) ^ 2) %>%
        dplyr::mutate(g1 = "b", g2 = "y")
    )
  dat %<>% dplyr::mutate(g3 = seq(nrow(dat)))

  dat_normalized <-
    dat %>%
    dplyr::select(g1, g2, g3, V1, V2) %>%
    dplyr::rename(x = V1, y = V2)

  dat <-
    dat %>%
    dplyr::select(g1, g2, g3, V3, V4) %>%
    dplyr::rename(x = V3, y = V4)

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  expect_less_than(
    norm(
      normalize(population = dat,
                variables = c("x", "y"),
                strata = c("g1", "g2"),
                sample = dat,
                operation = "standardize") %>%
        dplyr::collect() %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix() -
      dat_normalized %>%
        dplyr::arrange(g3) %>%
        dplyr::select(x, y) %>%
        as.matrix()
      ),
    .Machine$double.eps * 1000000
  )

})
