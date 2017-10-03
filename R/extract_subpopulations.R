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
#' \code{ctrl_clusters}).

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
#' @importFrom rlang .data
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
    type_var <- rlang::sym(type_var_name)

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
      dplyr::mutate(cluster_id = kmeans_outp$cluster) %>%
      dplyr::mutate(row_num = 1:n())  %>%
      dplyr::group_by_at(vars(dplyr::one_of(c("row_num",
                                       "cluster_id",
                                       type_var_name,
                                       non_feats)))) %>%
      dplyr::do(data.frame(dist_to_cluster =
                             find_dist_to_cluster(.[, ],
                                                  variables,
                                                  kmeans_outp,
                                                  "cluster_id"))) %>%
      dplyr::ungroup()

    subpop_profiles <- population %>%
      dplyr::group_by_(.dots = c(type_var_name, "cluster_id")) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::group_by_(.dots = type_var_name) %>%
      dplyr::rename(freq = "n") %>%
      dplyr::mutate(freq = .data$freq / sum(.data$freq) ) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = type_var_name, value = "freq", fill = 0)

    trt_clusters <- population %>%
      dplyr::filter(rlang::UQ(type_var) == "treatment") %>%
      dplyr::select(dplyr::one_of(c(non_feats,
                             "cluster_id",
                             "dist_to_cluster")))

    ctrl_clusters <- population %>%
      dplyr::filter(rlang::UQ(type_var) == "control") %>%
      dplyr::select(dplyr::one_of(c(non_feats,
                             "cluster_id",
                             "dist_to_cluster")))

    return(list(subpop_centers = kmeans_outp$centers,
                subpop_profiles = subpop_profiles,
                treatment_clusters = trt_clusters,
                ctrl_clusters = ctrl_clusters))
}
