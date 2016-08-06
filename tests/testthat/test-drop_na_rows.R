context("drop_na_rows")

test_that("`drop_na_rows` removes rows have only NAs", {

  dat <-
    data.frame(x = c(1, NA, 3, 4), y = c(1, NA, 3, NA)) %>%
    tibble::rownames_to_column()

  dat <- dplyr::copy_to(dplyr::src_sqlite(":memory:", create = T),
                        dat)

  dat %<>% dplyr::filter(x != 1)

  drop_na_rows.data.frame <- function(population, variables) {
    population %>%
      tidyr::gather_("key", "value", variables) %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::spread(key, value)
  }

  expect_equal(
    drop_na_rows(population = dat,
                 variables = c("x", "y")) %>%
      dplyr::collect() %>%
      dplyr::arrange(rowname),
    dat %>%
      dplyr::collect() %>%
      drop_na_rows.data.frame(variables = c("x", "y")) %>%
      dplyr::arrange(rowname)
  )

  expect_equal(
    drop_na_rows(population = dat,
                 variables = c("x")) %>%
      dplyr::collect() %>%
      dplyr::arrange(rowname),
    dat %>%
      dplyr::collect() %>%
      drop_na_rows.data.frame(variables = c("x")) %>%
      dplyr::arrange(rowname)
  )

})
