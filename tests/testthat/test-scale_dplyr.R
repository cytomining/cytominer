context("scale_dlpyr")

test_that("scale_dplyr works with sqlite", {
  set.seed(123)

  dat <- data.frame(a = rnorm(10), b = rnorm(10))

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = TRUE),
                        dat)

  mu <-
    dat %>%
    dplyr::summarise_each(dplyr::funs(mean)) %>%
    dplyr::collect()

  sigma <-
    dat %>%
    dplyr::summarise_each(dplyr::funs(sd)) %>%
    dplyr::collect()

  norm_1 <-
    dat %>%
    dplyr::collect() %>%
    dplyr::mutate_each(dplyr::funs(. - mu$.)) %>%
    dplyr::mutate_each(dplyr::funs(. / sigma$.))

  norm_2 <-
    scale_dplyr(dat, mu, sigma, vars = c("a", "b"))

  testthat::expect_true(
    norm(as.matrix(norm_2 %>% dplyr::collect() - norm_1)) <
      .Machine$double.eps * 1000
  )

})
