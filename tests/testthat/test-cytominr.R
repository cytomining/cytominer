test_that("cytominr", {
  fixture <-
    system.file("extdata", "fixture_intensities.sqlite", package = "cytominr")

  measurements <-
    dplyr::tbl(src = dplyr::src_sqlite(path = fixture), "measurements")

  features <- c(
    "integrated",
    "maximum",
    "mean",
    "median",
    "median_absolute_deviation",
    "minimum",
    "standard_deviation",
    "third_quartile"
  )

  normalized <- normalize(
    population = measurements %>%
      dplyr::filter(
        well_description %in% c(
          "A01",
          "A02",
          "A03",
          "A04"
        )
      ) %>%
      dplyr::select_(
        .dots = features
      ),
    sample = measurements %>%
      dplyr::filter(
        well_description %in% c(
          "A01",
          "A02"
        )
      ) %>%
      dplyr::select_(
        .dots = features
      ),
    operation = "standardize"
  )

  transformed <- transform(
    population = normalized,
    variables = features,
    operation = "generalized_log",
    c = 1
  )

  expect_equal(transformed %>% dplyr::collect() %>% nrow(), 44631)
})
