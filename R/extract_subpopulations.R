#' extract the subpopulations enriched/de-enriched in a given single cell population of a treatment w.r.t the control
#' 
#' @param population_treatment data frame which contains single cell data for a given treatment (cells are arranged in rows and measurements in columns)
#' @param population_control the same data frame (with the same column names, but possibly with different number of rows) for the negative control
#' @param feats a vector containing the feature names
#' @param k a scalar which is the number of subpopulations
#' 
#' @return list containing subpop. signatures (subpop_centers), two histograms showing freq. of each subpop. in treatment and control (subpop_profiles), and cluster prediction and distance to the predicted cluster for all input data (treatment_clusters and ctrl_clusters).
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr one_of
#' @importFrom dplyr vars
#' @importFrom stats setNames
#' 
#' @export
#' 
extract_subpopulations <- 
  function(population_treatment, 
           population_control, 
           feats,
           k) {
    
    non_feats <- setdiff(colnames(population_treatment), feats)
    
    population <- population_treatment %>% 
      dplyr::mutate(Metadata_Type = "treatment") %>%
      dplyr::bind_rows(., population_control %>% 
                         dplyr::mutate(Metadata_Type = "control")) 
    
    kmeans_outp <- population %>%
      dplyr::select(one_of(feats)) %>%
      stats::kmeans(centers = k, 
                    iter.max = 5000, 
                    nstart = 10)
    
    find_dist_to_cluster <- function(x, feats, kmeans_outp) {
      as.matrix(stats::dist(rbind(x[, feats], 
                 kmeans_outp$centers[x$Metadata_Cluster[1], feats])))[1, 2]
    }
      
    population %<>% 
      dplyr::mutate(Metadata_Cluster = kmeans_outp$cluster) %>%
      dplyr::mutate(row_number = 1:n())  %>%
      dplyr::group_by_at(vars(one_of(c("row_number", 
                                       "Metadata_Cluster", 
                                       "Metadata_Type", 
                                       non_feats)))) %>%
      dplyr::do(data.frame(Metadata_dist_to_cluster = find_dist_to_cluster(.[, ], feats, kmeans_outp))) %>%
      dplyr::ungroup()
    
    subpop_profiles <- population %>%
      dplyr::group_by_(.dots = c("Metadata_Type", "Metadata_Cluster")) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by_(.dots = "Metadata_Type") %>%
      dplyr::rename_('freq' = 'n') %>%
      dplyr::mutate_(.dots = setNames(list(~ freq / sum(freq)), "freq")) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = "Metadata_Type", value = "freq")
    
   
    trt_clusters <- population %>% 
      dplyr::filter_('Metadata_Type == "treatment"') %>% 
      dplyr::select(one_of(c(non_feats, 
                             "Metadata_Cluster",
                             "Metadata_dist_to_cluster")))
    
    ctrl_clusters <- population %>% 
      dplyr::filter_('Metadata_Type == "control"') %>% 
      dplyr::select(one_of(c(non_feats, 
                             "Metadata_Cluster",
                             "Metadata_dist_to_cluster")))

    return(list(subpop_centers = kmeans_outp$centers,
                subpop_profiles = subpop_profiles,
                treatment_clusters = trt_clusters,
                ctrl_clusters = ctrl_clusters))
  }