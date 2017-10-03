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

    data <-
      dplyr::bind_rows(
        population %>% dplyr::mutate(type = "population"),
        reference %>% dplyr::mutate(type = "reference")) %>%
      tidyr::drop_na(dplyr::one_of(variables))

    kmeans_output <- data %>%
      dplyr::select(dplyr::one_of(variables)) %>%
      stats::kmeans(centers = k,
                    iter.max = 5000,
                    nstart = 10)

    find_dist_to_cluster <- function(x) {
        (rbind(
          x[, variables],
          kmeans_output$centers[x[["cluster_id"]][1], variables]
          ) %>%
          stats::dist() %>%
          as.matrix())[1, 2]

    }

    data %<>% dplyr::mutate(cluster_id = kmeans_output$cluster)
    data %<>%
      dplyr::bind_cols(
        purrr::map_df(1:nrow(data),
               ~dplyr::data_frame(dist_to_cluster =
                                    find_dist_to_cluster(data[.x, ])))
      )

    subpop_profiles <- data %>%
      dplyr::group_by_(.dots = c("type", "cluster_id")) %>%
      dplyr::tally() %>%
      dplyr::group_by_(.dots = "type") %>%
      dplyr::rename(freq = "n") %>%
      dplyr::mutate(freq = .data$freq / sum(.data$freq) ) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = "type", value = "freq", fill = 0)

    # to avoid "no visible binding for global variable" warning
    # when doing select(-type) below
    type <- rlang::sym("type")

    population_clusters <-
      data %>%
      dplyr::filter(type == "population") %>%
      dplyr::select(-(!!type)) %>%
      dplyr::select(-dplyr::one_of(variables))

    reference_clusters <-
      data %>%
      dplyr::filter(type == "reference") %>%
      dplyr::select(-(!!type)) %>%
      dplyr::select(-dplyr::one_of(variables))

    return(list(subpop_centers = kmeans_output$centers,
                subpop_profiles = subpop_profiles,
                population_clusters = population_clusters,
                reference_clusters = reference_clusters))
}
