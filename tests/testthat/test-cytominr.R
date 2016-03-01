test_that("cytominr", {

  fixture <-
    system.file("extdata", "fixture_intensities.sqlite", package = "cytominr")

  measurements <-
    dplyr::tbl(src = dplyr::src_sqlite(path = fixture), "measurements")

  features <-
    c(
      "integrated",
      "maximum",
      "mean",
      "median",
      "median_absolute_deviation",
      "minimum",
      "standard_deviation",
      "third_quartile"
    )

  normalized <-
    normalize(
      population = measurements %>% dplyr::filter(well_description %in% c("A01", "A02", "A03", "A04")),
      variables = features,
      sample = measurements %>% dplyr::filter(well_description %in% c("A01","A02"))
    )

  transformed <-
    transform(
      population = normalized,
      variables = features
    )

  selected <-
    select(
      population = transformed %>% dplyr::collect(),
      variables = features,
      sample = transformed %>% dplyr::collect()
    )

  expect_equal(selected %>% dplyr::collect() %>% nrow(), 44631)
})
