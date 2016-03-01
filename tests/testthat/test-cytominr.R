test_that("cytominr", {
  fixture <-
    system.file("extdata", "fixture_intensities.sqlite", package = "cytominr")

  measurements <-
    dplyr::tbl(src = dplyr::src_sqlite(path = fixture), "measurements")

  metadata <- c("plate_barcode",
                "well_description",
                "pattern_description",
                "channel_description")
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

  aggregated <-
    aggregate(
      population = transformed,
      variables = features,
      grouping_variables = metadata
    ) %>%
    dplyr::collect()

  selected <-
    select(
      population = transformed ,
      variables = features,
      sample = aggregated
    )

  expect_equal(selected %>% dplyr::collect() %>% nrow(), 44631)
})
