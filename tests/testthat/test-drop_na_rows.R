context("drop_na_rows")

test_that("`drop_na_rows` removes rows have only NAs", {

  data <-
    data.frame(x = c(1, NA, 3, 4), y = c(1, NA, 3, NA)) %>%
    tibble::rownames_to_column()

  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  data <- dplyr::copy_to(db, data)

  data %<>% dplyr::filter(x != 1)

  drop_na_rows_data_frame <- function(population, variables) {
    population %>%
      tidyr::gather_("key", "value", variables) %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::spread(key, value)
  }

  expect_equal(
    drop_na_rows(population = data,
                 variables = c("x", "y")) %>%
      dplyr::collect() %>%
      dplyr::arrange(rowname),
    data %>%
      dplyr::collect() %>%
      drop_na_rows_data_frame(variables = c("x", "y")) %>%
      dplyr::arrange(rowname)
  )

  expect_equal(
    drop_na_rows(population = data,
                 variables = c("x")) %>%
      dplyr::collect() %>%
      dplyr::arrange(rowname),
    data %>%
      dplyr::collect() %>%
      drop_na_rows_data_frame(variables = c("x")) %>%
      dplyr::arrange(rowname)
  )

  # repeat tests with data frames instead of sql tables
  expect_equal(
    drop_na_rows(population = data %>% dplyr::collect(),
      variables = c("x", "y")) %>%
      dplyr::arrange(rowname),
    data %>%
      dplyr::collect() %>%
      drop_na_rows_data_frame(variables = c("x", "y")) %>%
      dplyr::arrange(rowname)
  )

  expect_equal(
    drop_na_rows(population = data %>% dplyr::collect(),
      variables = c("x")) %>%
      dplyr::arrange(rowname),
    data %>%
      dplyr::collect() %>%
      drop_na_rows_data_frame(variables = c("x")) %>%
      dplyr::arrange(rowname)
  )

  DBI::dbDisconnect(db)
})
