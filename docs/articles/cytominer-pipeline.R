## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(magrittr)
futile.logger::flog.threshold(futile.logger::WARN)

## ------------------------------------------------------------------------
fixture <-
  system.file("extdata", "fixture_intensities_shapes.sqlite",
              package = "cytominer")

db <- DBI::dbConnect(RSQLite::SQLite(), fixture)
  

## ------------------------------------------------------------------------
ext_metadata <-
  readr::read_csv(system.file("extdata", "metadata.csv",
                              package = "cytominer")) %>%
  dplyr::rename(g_well = Well)

ext_metadata <- dplyr::copy_to(db, ext_metadata)


## ------------------------------------------------------------------------
intensities <-
  dplyr::tbl(src = db, "view_intensities") %>%
  dplyr::compute()


## ------------------------------------------------------------------------
measurements <-
  intensities %>%
  dplyr::filter(g_well %in% c("A01", "A02", "A10", "A11"))

## ------------------------------------------------------------------------
measurements %>%
  dplyr::tally() %>%
  knitr::kable()

## ------------------------------------------------------------------------
qc_cols <- c("q_debris")

group_cols <-
  c("g_plate",
    "g_well",
    "g_image",
    "g_pattern",
    "g_channel")

feature_cols <-
  colnames(measurements) %>%
  stringr::str_subset("^m_")

measurements %<>%
  dplyr::select(dplyr::one_of(c(group_cols, qc_cols, feature_cols)))


## ------------------------------------------------------------------------
debris_removed <-
  measurements %>% dplyr::filter(q_debris == 0)

## ------------------------------------------------------------------------
na_rows_removed <-
  cytominer::drop_na_rows(
    population = debris_removed,
    variables = feature_cols
  ) %>%
  dplyr::compute()

## ------------------------------------------------------------------------
normalized <-
  cytominer::normalize(
    population = na_rows_removed %>% 
      dplyr::collect(),
    variables = feature_cols,
    strata =  c("g_plate", "g_pattern", "g_channel"),
    sample =
      na_rows_removed %>%
      dplyr::inner_join(
        ext_metadata %>% 
          dplyr::filter(Type == "ctrl") %>% 
          dplyr::select(g_well) 
      ) %>% dplyr::collect()
  )

normalized %<>% dplyr::collect()

## ------------------------------------------------------------------------
na_frequency <-
  cytominer::count_na_rows(
    population = normalized,
    variables = feature_cols)

na_frequency %>%
  tidyr::gather(feature, na_count) %>%
  knitr::kable()


## ------------------------------------------------------------------------

cleaned <-
  cytominer::select(
    population = normalized,
    variables = feature_cols,
    operation = "drop_na_columns"
)

## ------------------------------------------------------------------------
transformed <-
  cytominer::transform(
    population = cleaned,
    variables = feature_cols
  )

## ------------------------------------------------------------------------
aggregated <-
  cytominer::aggregate(
    population = transformed,
    variables = feature_cols,
    strata = group_cols
  ) %>%
  dplyr::collect()

## ------------------------------------------------------------------------
selected <-
  cytominer::select(
    population = transformed,
    variables = feature_cols,
    sample = aggregated,
    operation = "correlation_threshold"
  ) %>%
  dplyr::collect()

## ------------------------------------------------------------------------
selected %>%
  dplyr::glimpse()

## ------------------------------------------------------------------------
  DBI::dbDisconnect(db)

