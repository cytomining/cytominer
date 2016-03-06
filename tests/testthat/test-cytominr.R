test_that("cytominr", {
  fixture <-
    system.file("extdata", "fixture_intensities_shapes.sqlite", package = "cytominr")

  db <- dplyr::src_sqlite(path = fixture)

  ext_metadata <- readr::read_csv(system.file("extdata", "metadata.csv", package = "cytominr")) %>% dplyr::rename(g_well = Well)

  ext_metadata <- dplyr::copy_to(db, ext_metadata)

  intensities <-
    dplyr::tbl(src = db, "view_intensities")

  measurements <-
    intensities %>%
    dplyr::filter(g_pattern == "Nuclei") %>%
    dplyr::filter(g_channel %in% c("CellMask", "Hoechst"))

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
    dplyr::select(one_of(c(group_cols, qc_cols, feature_cols))) %>%
    dplyr::filter(g_well %in% c("A01", "A02", "A03", "A08"))

  debris_removed <-
    measurements %>% dplyr::filter(q_debris == 0)

  # the join below will be slow without an index on g_well
  normalized <-
    normalize(
      population = debris_removed,
      variables = feature_cols,
      grouping_variables =  c("g_plate", "g_pattern", "g_channel"),
      sample =
        debris_removed %>%
        dplyr::inner_join(
          ext_metadata %>% dplyr::filter(Type == "ctrl") %>% dplyr::select(g_well)
        )
    )

  # TODO: this should be moved into a unit test for normalize
  expect_less_than(
    normalized %>%
    dplyr::filter(g_well == "A08") %>%
    dplyr::group_by_(.dots = c("g_plate", "g_pattern", "g_channel")) %>%
    dplyr::summarise_each_(dplyr::funs(mean), vars = feature_cols) %>%
    dplyr::collect() %>%
    dplyr::ungroup() %>%
    dplyr::select_(.dots = feature_cols) %>%
    tidyr::gather(key, value) %>%
    dplyr::summarize(value = max(value)),
    1000* .Machine$double.eps
  )

  transformed <-
    transform(
      population = normalized,
      variables = feature_cols
    )

  aggregated <-
    aggregate(
      population = transformed,
      variables = feature_cols,
      grouping_variables = group_cols
    ) %>%
    dplyr::collect()

  selected <-
    select(
      population = transformed ,
      variables = feature_cols,
      sample = aggregated
    )

  # Number of objects in images that have debris = 1248
  expect_equal(selected %>% dplyr::collect() %>% nrow(), 9922 - 1248)
})
