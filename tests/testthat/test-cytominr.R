test_that('cytominr', {
  database <-
    dplyr::src_sqlite(path =
                        system.file("extdata", "fixture_intensities.sqlite",
                                    package = "cytominr")
                      )

  measurements <- dplyr::tbl(database, 'measurements')

  metadata <- c(
    'plate_barcode',
    'well_description',
    'image_description',
    'object_description',
    'pattern_description',
    'channel_description'
  )

  features <- c(
    'integrated',
    'maximum',
    'mean',
    'median',
    'median_absolute_deviation',
    'minimum',
    'standard_deviation',
    'third_quartile'
  )

  population <- measurements %>%
    dplyr::filter(
      well_description %in%
        c(
          "A01",
          "A02",
          "A03",
          "A04"
        )
    ) %>%
    dplyr::select_(.dots = features)

  sample <- measurements %>%
    dplyr::filter(
      well_description %in%
        c(
          "A01",
          "A02"
        )
    ) %>%
    dplyr::select_(
      .dots = features
    )

  normalized <- normalize(
    population = population,
    sample = sample,
    operation = 'standardize'
  )

  expect_equal(normalized %>%
                 dplyr::collect() %>%
                 nrow(),
               44631)
})
