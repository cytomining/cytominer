#' extract the subpopulations enriched/de-enriched in a given single cell population of a treatment w.r.t the control
#' 
#' @param population_treatment single cell data for a given treatment, including just the features
#' @param population_control the same matrix for the negative control
#' @param k number of subpopulations
#' 
#' @return list containing subpop. signatures and two histograms showing freq. of each subpop. in treatment and control.
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr one_of
#' 
#' @export
#' 
extract_subpopulations <- 
  function(population_treatment, 
           population_control, 
           k) {
    
    feats <- colnames(population_treatment)
    
    population <- population_treatment %>% 
      dplyr::mutate(Metadata_Type = "treatment") %>%
      dplyr::bind_rows(., population_control %>% 
                         dplyr::mutate(Metadata_Type = "control")) 
    
    kmeans_outp <- population %>%
      dplyr::select(one_of(feats)) %>%
      stats::kmeans(centers = k, 
                    iter.max = 5000, 
                    nstart = 10)
    
    population %<>% 
      dplyr::mutate(Metadata_Cluster = kmeans_outp$cluster)
    
    subpop_profiles <- population %>%
      dplyr::group_by(Metadata_Type, Metadata_Cluster) %>%
      dplyr::summarise(n = n())
    
    subpop_profiles %<>%
      dplyr::group_by(Metadata_Type) %>%
      dplyr::rename(freq = n) %>%
      dplyr::mutate(freq = freq / sum(freq)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = "Metadata_Type", value = "freq")
    
    trt_clusters <- population %>% 
      dplyr::filter(Metadata_Type == "treatment") %>% 
      dplyr::select(Metadata_Cluster)
    
    ctrl_clusters <- population %>% 
      dplyr::filter(Metadata_Type == "control") %>% 
      dplyr::select(Metadata_Cluster)
    
    return(list(subpop_centers = kmeans_outp$centers,
                subpop_profiles = subpop_profiles,
                treatment_clusters = trt_clusters,
                ctrl_clusters = ctrl_clusters))
  }