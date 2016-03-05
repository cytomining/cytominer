test_that("aggregated intensities is valid", {
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

  aggregated_intensities <-
    aggregate(population = fixture_intensities,
              variables = paste("Intensity", features, sep = "_"),
              grouping_variables = metadata)

  fixture_aggregated_intensities <-
    fixture_intensities %>%
    dplyr::group_by_(.dots = metadata) %>%
    dplyr::summarise_each_(dplyr::funs(mean), vars = paste("Intensity", features, sep = "_")) %>%
    dplyr::ungroup()

  a <- aggregated_intensities %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  b <- fixture_aggregated_intensities %>% dplyr::select(-one_of(metadata)) %>% as.matrix()

  expect_equal(a, b)

})
