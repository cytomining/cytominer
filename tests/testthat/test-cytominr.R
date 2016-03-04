test_that("cytominr", {
  fixture <-
    system.file("extdata", "fixture_intensities.sqlite", package = "cytominr")

  intensities <-
    dplyr::tbl(src = dplyr::src_sqlite(path = fixture), "view_intensities")

  metadata <- c("plate_description",
                "well_description",
                "pattern_description",
                "channel_description")
  features <-
    c(
      "intensity_integrated",
      "intensity_maximum",
      "intensity_mean",
      "intensity_median",
      "intensity_median_absolute_deviation",
      "intensity_minimum",
      "intensity_standard_deviation",
      "intensity_third_quartile"
    )

  normalized <-
    normalize(
      population = intensities %>% dplyr::filter(well_description %in% c("A01", "A02", "A03", "A04")),
      variables = features,
      sample = intensities %>% dplyr::filter(well_description %in% c("A01","A02"))
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
