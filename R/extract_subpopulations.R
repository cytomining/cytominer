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
    
    type_var_name <- "pert_type"
    cluster_var_name <- "cluster_id"
    dist_var_name <- "dist_to_cluster"
    freq_var_name <- "freq"
    row_var_name <- "row_number"
    type_var <- rlang::sym(type_var_name)
    freq_var <- rlang::sym(freq_var_name)

    population <- population_treatment %>% 
      dplyr::mutate(!!type_var_name := "treatment") %>%
      dplyr::bind_rows(., population_control %>% 
                         dplyr::mutate(!!type_var_name := "control")) 
    
    kmeans_outp <- population %>%
      dplyr::select(one_of(feats)) %>%
      stats::kmeans(centers = k, 
                    iter.max = 5000, 
                    nstart = 10)
    
    find_dist_to_cluster <- function(x, 
                                     feats, 
                                     kmeans_outp, 
                                     cluter_var_name) {
      as.matrix(stats::dist(rbind(x[, feats], 
                 kmeans_outp$centers[x[[cluter_var_name]][1], feats])))[1, 2]
    }
      
    population %<>% 
      dplyr::mutate(!!cluster_var_name := kmeans_outp$cluster) %>%
      dplyr::mutate(!!row_var_name := 1:n())  %>%
      dplyr::group_by_at(vars(one_of(c(row_var_name, 
                                       cluster_var_name, 
                                       type_var_name, 
                                       non_feats)))) %>%
      dplyr::do(data.frame("name_tmp" = find_dist_to_cluster(.[, ], 
                                                          feats, 
                                                          kmeans_outp, 
                                                          cluster_var_name))) %>%
      dplyr::ungroup() %>%
      dplyr::rename(!!dist_var_name := !!"name_tmp")
    
    subpop_profiles <- population %>%
      dplyr::group_by_(.dots = c(type_var_name, cluster_var_name)) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by_(.dots = type_var_name) %>%
      dplyr::rename(!!freq_var := !!"n") %>%
      dplyr::mutate(!!freq_var := ((!!freq_var) / sum(!!freq_var))) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = type_var_name, value = freq_var_name)
    
   
    trt_clusters <- population %>% 
      dplyr::filter(rlang::UQ(type_var) == "treatment") %>% 
      dplyr::select(one_of(c(non_feats, 
                             cluster_var_name,
                             dist_var_name)))
    
    ctrl_clusters <- population %>% 
      dplyr::filter(rlang::UQ(type_var) == "control") %>% 
      dplyr::select(one_of(c(non_feats, 
                             cluster_var_name,
                             dist_var_name)))
    
    return(list(subpop_centers = kmeans_outp$centers,
                subpop_profiles = subpop_profiles,
                treatment_clusters = trt_clusters,
                ctrl_clusters = ctrl_clusters))
  }