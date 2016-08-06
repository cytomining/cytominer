context("cytominr integration test")

test_that("cytominr", {

  futile.logger::flog.threshold(futile.logger::WARN)

  fixture <-
    system.file("extdata", "fixture_intensities_shapes.sqlite",
                package = "cytominr")

  db <- dplyr::src_sqlite(path = fixture)

  ext_metadata <-
    readr::read_csv(system.file("extdata", "metadata.csv",
                                package = "cytominr")) %>%
    dplyr::rename(g_well = Well)

  ext_metadata <- dplyr::copy_to(db, ext_metadata)

  futile.logger::flog.info("Creating temp table for intensities")
  intensities <-
    dplyr::tbl(src = db, "view_intensities") %>%
    dplyr::compute()
  futile.logger::flog.info("Created temp table for intensities")

  measurements <-
    intensities %>%
    dplyr::filter(g_well %in% c("A01", "A02", "A10", "A11"))

  qc_cols <- c("q_debris")

  group_cols <-
    c("g_plate",
      "g_well",
      "g_image",
      "g_pattern",
      "g_channel")

  testthat::expect_true(all(group_cols %in% (
    colnames(measurements) %>%
      stringr::str_subset("^g_")
  )))

  testthat::expect_true(all(qc_cols %in% (
    colnames(measurements) %>%
      stringr::str_subset("^q_")
  )))

  feature_cols <-
    colnames(measurements) %>%
    stringr::str_subset("^m_")

  measurements %<>%
    dplyr::select(dplyr::one_of(c(group_cols, qc_cols, feature_cols)))

  # data cleaning
  debris_removed <-
    measurements %>% dplyr::filter(q_debris == 0)

  na_rows_removed <-
    drop_na_rows(
      population = debris_removed,
      variables = feature_cols
    ) %>%
    dplyr::compute()

  # normalization (standardization by default)
  futile.logger::flog.info("Normalizing")
  normalized <-
    normalize(
      population = na_rows_removed,
      variables = feature_cols,
      strata =  c("g_plate", "g_pattern", "g_channel"),
      sample =
        debris_removed %>%
        dplyr::inner_join(
          ext_metadata %>% dplyr::filter(Type == "ctrl") %>%
            dplyr::select(g_well)
        )
    )
  futile.logger::flog.info("Normalized")

  # calculate frequency of NAs per variable
  na_frequency <-
    count_na_rows(
      population = normalized,
      variables = feature_cols)

  # drop NA columns because they may arise after normalize
  cleaned <-
    select(
      population = normalized,
      variables = feature_cols,
      operation = "drop_na_columns"
  )

  # tranformation (generalized log by default)
  transformed <-
    transform(
      population = cleaned,
      variables = feature_cols
    )

  # aggregation (mean by default)
  aggregated <-
    aggregate(
      population = transformed,
      variables = feature_cols,
      strata = group_cols
    ) %>%
    dplyr::collect()

  # feature selection (variance threshold by default)
  selected <-
    select(
      population = transformed,
      variables = feature_cols,
      sample = aggregated,
      operation = "correlation_threshold"
    ) %>%
    dplyr::collect()

})
