#' extract the subpopulations enriched/de-enriched in a given single cell population of a treatment w.r.t the control
#'
#' @param population_treatment data frame which contains single cell data for a given treatment (cells are arranged in rows and measurements in columns)
#' @param population_control the same data frame (with the same column names, but possibly with different number of rows) for the negative control
#' @param feats a vector containing the feature names
#' @param k a scalar which is the number of subpopulations
#'
#' @return list containing subpop. signatures (subpop_centers), two histograms showing freq. of each subpop. in treatment and control (subpop_profiles), and cluster prediction and distance to the predicted cluster for all input data (treatment_clusters and ctrl_clusters).
#' @examples
#' population <- tibble::data_frame(
#'    Metadata_group = c("control", "control", "control", "control",
#'                       "experiment", "experiment", "experiment", "experiment"),
#'    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'    AreaShape_Area = c(10, 12, NA, 16, 8, 8, 7, 7),
#'    AreaShape_Length = c(2, 3, NA, NA, 4, 5, 1, 5)
#' )
#' variables <- c('AreaShape_Area','AreaShape_Length')
#' population_trt <-  dplyr::filter(population, Metadata_group == "experiment")
#' population_ctrl <- dplyr::filter(population, Metadata_group == "control")
#' extract_subpopulations(
#'    population_treatment = population_trt,
#'    population_control = population_ctrl,
#'    feats = variables,
#'    k = 3
#' )
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
                         dplyr::mutate(!!type_var_name := "control")) %>%
      tidyr::drop_na(one_of(feats))

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
      tidyr::spread(key = type_var_name, value = freq_var_name, fill = 0)

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
