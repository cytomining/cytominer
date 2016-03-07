test_that("Remove NA works", {
  fixture <-
    system.file("extdata", "fixture_intensities_shapes.sqlite", package = "cytominr")

  db <- dplyr::src_sqlite(path = fixture)

  shapes <-
    dplyr::tbl(src = db, "view_shapes")

  measurements <-
    shapes %>%
    dplyr::filter(g_pattern == "Nuclei")

  group_cols <-
    c("g_plate",
      "g_well",
      "g_image",
      "g_pattern")

  feature_cols <-
    colnames(measurements) %>%
    stringr::str_subset("^m_")

  measurements %<>%
    dplyr::select(one_of(c(group_cols, feature_cols))) %>%
    dplyr::filter(g_well == "A08")

  normalized <-
    normalize(
      population = measurements,
      variables = feature_cols,
      grouping_variables =  c("g_plate", "g_pattern"),
      sample = measurements
    )

  a <-
    cytominr::drop_na_columns(
      population = normalized,
      variables = feature_cols
    )

  expect_equal(a, c("m_shapes_euler_number"))

})
