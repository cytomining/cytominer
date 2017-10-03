#' Extract subpopulations.
#'
#' \code{extract_subpopulations} extracts the subpopulations enriched/de-enriched in a given set w.r.t a reference set
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param reference tbl with grouping (metadata) and observation variables. Columns of \code{population} and \code{reference} should be identical.
#' @param variables character vector specifying observation variables.
#' @param k scalar specifying number of subpopulations
#'
#' @return list containing subpopulation signatures (\code{subpop_centers}), two
#' histograms specifying frequency of each subpopulation in population and
#' reference (\code{subpop_profiles}), and cluster prediction and distance to
#' the predicted cluster for all input data (\code{treatment_clusters} and
#' \code {ctrl_clusters}).

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
#'    population = population_trt,
#'    reference = population_ctrl,
#'    variables = variables,
#'    k = 3
#' )
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr vars
#' @importFrom stats setNames
#'
#' @export
#'
extract_subpopulations <-
  function(population,
           reference,
           variables,
           k) {

    non_feats <- setdiff(colnames(population), variables)

    type_var_name <- "pert_type"
    cluster_var_name <- "cluster_id"
    dist_var_name <- "dist_to_cluster"
    freq_var_name <- "freq"
    row_var_name <- "row_number"
    type_var <- rlang::sym(type_var_name)
    freq_var <- rlang::sym(freq_var_name)

    population <- population %>%
      dplyr::mutate(!!type_var_name := "treatment") %>%
      dplyr::bind_rows(., reference %>%
                         dplyr::mutate(!!type_var_name := "control")) %>%
      tidyr::drop_na(dplyr::one_of(variables))

    kmeans_outp <- population %>%
      dplyr::select(dplyr::one_of(variables)) %>%
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
      dplyr::group_by_at(vars(dplyr::one_of(c(row_var_name,
                                       cluster_var_name,
                                       type_var_name,
                                       non_feats)))) %>%
      dplyr::do(data.frame("name_tmp" =
                             find_dist_to_cluster(.[, ],
                                                  variables,
                                                  kmeans_outp,
                                                  cluster_var_name))) %>%
      dplyr::ungroup() %>%
      dplyr::rename(!!dist_var_name := !!"name_tmp")

    subpop_profiles <- population %>%
      dplyr::group_by_(.dots = c(type_var_name, cluster_var_name)) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by_(.dots = type_var_name) %>%
      dplyr::rename(!!freq_var := !!"n") %>%
      dplyr::mutate(!!freq_var := ( (!!freq_var) / sum(!!freq_var)) ) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = type_var_name, value = freq_var_name, fill = 0)

    trt_clusters <- population %>%
      dplyr::filter(rlang::UQ(type_var) == "treatment") %>%
      dplyr::select(dplyr::one_of(c(non_feats,
                             cluster_var_name,
                             dist_var_name)))

    ctrl_clusters <- population %>%
      dplyr::filter(rlang::UQ(type_var) == "control") %>%
      dplyr::select(dplyr::one_of(c(non_feats,
                             cluster_var_name,
                             dist_var_name)))

    return(list(subpop_centers = kmeans_outp$centers,
                subpop_profiles = subpop_profiles,
                treatment_clusters = trt_clusters,
                ctrl_clusters = ctrl_clusters))
}
