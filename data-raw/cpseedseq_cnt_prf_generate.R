# Load cpseedseq dataset as a profile.data object
cfg_fname <- "inst/extdata/well-summary-profile_mean-cellcount-dummy-dummy_norm.yml"
cpseedseq_cnt_prf <- pertminr:::profile.data(cfg_fname)
cpseedseq_cnt_prf %<>% pertminr:::process_metadata.profile.data(strip_cellprofiler_db_tags = T)
cpseedseq_cnt_prf$metadata %<>% dplyr::filter(Plate %in% c(38034, 38003, 37983))
cpseedseq_cnt_prf %<>% pertminr:::post_filter_metadata.profile.data()
devtools::use_data(cpseedseq_cnt_prf)
