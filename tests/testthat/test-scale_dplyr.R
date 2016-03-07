test_that("scale_dplyr is valid", {
  set.seed(123)

  my_db <- dplyr::src_sqlite(":memory:", create = TRUE)

  tmp_df <- data.frame(a = rnorm(10), b = rnorm(10))
  smpl <- dplyr::copy_to(my_db,
                         tmp_df,
                         temporary = FALSE)

  mu <-
    smpl %>%
    dplyr::summarise_each(dplyr::funs(mean)) %>%
    dplyr::collect()

  sigma <-
    smpl %>%
    dplyr::summarise_each(dplyr::funs(sd)) %>%
    dplyr::collect()

  smpl_norm_1 <-
    smpl %>%
    dplyr::collect() %>%
    dplyr::mutate_each(dplyr::funs(. - mu$.)) %>%
    dplyr::mutate_each(dplyr::funs(. / sigma$.))

  smpl_norm_2 <-
    scale_dplyr(smpl, mu, sigma, vars = c("a", "b"))

  smpl_norm_2 %>%
    dplyr::explain()

  testthat::expect_true(
    norm(as.matrix(smpl_norm_2 %>% dplyr::collect() - smpl_norm_1)) <
      .Machine$double.eps * 1000
  )

})
