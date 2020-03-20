context("cytominer integration test")

test_that("cytominer can process dataset with a normalized schema", {
  skip_on_os("windows")

  futile.logger::flog.threshold(futile.logger::WARN)

  fixture <-
    system.file(
      "extdata", "fixture_intensities_shapes.sqlite",
      package = "cytominer"
    )

  db <- dplyr::src_sqlite(path = fixture)

  ext_metadata <-
    readr::read_csv(system.file(
      "extdata", "metadata.csv",
      package = "cytominer"
    )) %>%
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

  qualities <- c("q_debris")

  groupings <-
    c(
      "g_plate",
      "g_well",
      "g_image",
      "g_pattern",
      "g_channel"
    )

  testthat::expect_true(all(groupings %in% (
    colnames(measurements) %>%
      stringr::str_subset("^g_")
  )))

  testthat::expect_true(all(qualities %in% (
    colnames(measurements) %>%
      stringr::str_subset("^q_")
  )))

  variables <-
    colnames(measurements) %>%
    stringr::str_subset("^m_")

  measurements %<>%
    dplyr::select(c(groupings, qualities, variables))

  # data cleaning
  debris_removed <-
    measurements %>%
    dplyr::filter(q_debris == 0)

  na_rows_removed <-
    drop_na_rows(
      population = debris_removed,
      variables = variables
    ) %>%
    dplyr::compute()

  # normalization (standardization by default)
  futile.logger::flog.info("Normalizing")
  normalized <-
    normalize(
      population = na_rows_removed,
      variables = variables,
      strata = c("g_plate", "g_pattern", "g_channel"),
      sample =
        na_rows_removed %>%
          dplyr::inner_join(
            ext_metadata %>%
              dplyr::filter(Type == "ctrl") %>%
              dplyr::select(g_well)
          )
    )
  futile.logger::flog.info("Normalized")

  # not doing this is resulting in "parser stack overflow" likely because
  # query becomes too long. dplyr::collect and dplyr::collapse don't help here.
  normalized %<>% dplyr::collect()

  # calculate frequency of NAs per variable
  na_frequency <-
    count_na_rows(
      population = normalized,
      variables = variables
    )

  # drop NA columns because they may arise after normalize
  cleaned <-
    variable_select(
      population = normalized,
      variables = variables,
      operation = "drop_na_columns"
    )

  # tranformation (generalized log by default)
  transformed <-
    transform(
      population = cleaned,
      variables = variables
    )

  # aggregation (mean by default)
  aggregated <-
    aggregate(
      population = transformed,
      variables = variables,
      strata = groupings
    ) %>%
    dplyr::collect()

  variables <-
    colnames(aggregated) %>%
    stringr::str_subset("^m_")

  # feature selection (variance threshold by default)
  selected <-
    variable_select(
      population = transformed,
      variables = variables,
      operation = "correlation_threshold",
      sample = aggregated
    ) %>%
    dplyr::collect()
})

test_that("cytominer can process dataset with a CellProfiler schema", {
  skip_on_os("windows")

  set.seed(123)

  futile.logger::flog.threshold(futile.logger::WARN)

  fixture <-
    system.file(
      "extdata", "fixture_htqc.sqlite",
      package = "cytominer"
    )

  db <- DBI::dbConnect(RSQLite::SQLite(), fixture)

  ext_metadata <-
    readr::read_csv(system.file(
      "extdata", "metadata.csv",
      package = "cytominer"
    )) %>%
    dplyr::rename(g_well = Well)

  ext_metadata <- dplyr::copy_to(db, ext_metadata)

  futile.logger::flog.info("Creating table for objects")

  image <- dplyr::tbl(src = db, "image")

  object <-
    dplyr::tbl(src = db, "cells") %>%
    dplyr::inner_join(
      dplyr::tbl(src = db, "cytoplasm"),
      by = c("TableNumber", "ImageNumber", "ObjectNumber")
    ) %>%
    dplyr::inner_join(
      dplyr::tbl(src = db, "nuclei"),
      by = c("TableNumber", "ImageNumber", "ObjectNumber")
    )

  object %<>% dplyr::inner_join(
    image %>%
      dplyr::select(
        "TableNumber",
        "ImageNumber",
        "image_Metadata_Barcode",
        "image_Metadata_Well",
        "image_Metadata_isDebris"
      ),
    by = c("TableNumber", "ImageNumber")
  )

  # need to rename individually because of
  # https://github.com/tidyverse/dplyr/issues/2860
  # https://github.com/tidyverse/dplyr/issues/2896
  object %<>% dplyr::rename(g_plate = "image_Metadata_Barcode")
  object %<>% dplyr::rename(g_well = "image_Metadata_Well")
  object %<>% dplyr::rename(g_table = "TableNumber")
  object %<>% dplyr::rename(g_image = "ImageNumber")
  object %<>% dplyr::rename(q_debris = "image_Metadata_isDebris")

  futile.logger::flog.info("Created table for objects")

  measurements <-
    object %>%
    dplyr::filter(g_well %in% c("A01", "A02", "A10", "A11"))

  qualities <- c("q_debris")

  groupings <-
    c(
      "g_plate",
      "g_well",
      "g_table",
      "g_image"
    )

  variables <-
    colnames(measurements) %>%
    stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

  testthat::expect_true(all(groupings %in% (
    colnames(measurements) %>%
      stringr::str_subset("^g_")
  )))

  testthat::expect_true(all(qualities %in% (
    colnames(measurements) %>%
      stringr::str_subset("^q_")
  )))

  variables <-
    colnames(measurements) %>%
    stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

  # select only a few variables to speed up test
  variables <- sample(variables, 10)

  measurements %<>%
    dplyr::select(c(groupings, qualities, variables))

  # data cleaning
  debris_removed <-
    measurements %>%
    dplyr::filter(q_debris == 0)

  # Coalesce can't handle the large number of columns so skipping the
  # `na_rows_removed` step
  na_rows_removed <- debris_removed

  # dplyr::collect is forced below for `population` and `sample`
  # not doing this is resulting in "parser stack overflow" likely because
  # query becomes too long. dplyr::compute and dplyr::collapse don't help here.

  # normalization (standardization by default)
  futile.logger::flog.info("Normalizing")
  normalized <-
    normalize(
      population = na_rows_removed %>% dplyr::collect(),
      variables = variables,
      strata = c("g_plate"),
      sample =
        na_rows_removed %>%
          dplyr::inner_join(
            ext_metadata %>%
              dplyr::filter(Type == "ctrl") %>%
              dplyr::select(g_well)
          ) %>%
          dplyr::collect()
    )
  futile.logger::flog.info("Normalized")

  # calculate frequency of NAs per variable
  na_frequency <-
    count_na_rows(
      population = normalized,
      variables = variables
    )

  # drop NA columns because they may arise after normalize
  cleaned <-
    variable_select(
      population = normalized,
      variables = variables,
      operation = "drop_na_columns"
    )

  variables <-
    colnames(cleaned) %>%
    stringr::str_subset("^Nuclei_|^Cells_|^Cytoplasm_")

  # tranformation (generalized log by default)
  transformed <-
    transform(
      population = cleaned,
      variables = variables
    )

  # aggregation (default is mean)
  aggregated <-
    aggregate(
      population = transformed,
      variables = variables,
      strata = groupings
    ) %>%
    dplyr::collect()

  variables <-
    colnames(aggregated) %>%
    stringr::str_subset("^m_")

  # feature selection (variance threshold by default)
  selected <-
    variable_select(
      population = transformed,
      variables = variables,
      sample = aggregated,
      operation = "correlation_threshold"
    ) %>%
    dplyr::collect()

  DBI::dbDisconnect(db)
})
