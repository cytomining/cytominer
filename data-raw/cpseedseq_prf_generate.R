# Load cpseedseq dataset as a profile.data object
cfg_fname <- "inst/extdata/well-summary-profile_mean-median-robust_std-untreated_norm.yml"
cpseedseq_prf <- pertminr::profile.data(cfg_fname)

cpseedseq_prf <- pertminr::process_metadata(cpseedseq_prf,
  strip_cellprofiler_db_tags = T)

cpseedseq_prf$metadata <- dplyr::filter(cpseedseq_prf$metadata,
                                        Plate %in% c(38034, 38003, 37983))

cpseedseq_prf <- post_filter_metadata(cpseedseq_prf)

devtools::use_data(cpseedseq_prf)
