#' Replicate correlation of variables
#'
#' @param sample ...
#' @param variables ...
#' @param strata ...
#' @param replicates ...
#' @param replicate_by ...
#' @param split_by ...
#'
#' @return data.frame of variable quality measurements
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom foreach %dopar%
#' @importFrom stats median
#'
#' @export
#'
replicate_correlation <-
  function(sample, variables, strata, replicates, replicate_by = NULL, split_by = NULL) {
    if (is.null(split_by)) {
      sample %<>% dplyr::mutate(batch = 0)

      split_by <- "col_split_by"
    }

    if (is.null(replicate_by)) {
      replicate_by <- "col_replicate_by"

      sample %<>%
        dplyr::count_(vars = strata) %>%
        dplyr::filter(n == replicates) %>%
        dplyr::select(-n) %>%
        dplyr::inner_join(sample) %>%
        dplyr::group_by_(.dots = strata) %>%
        dplyr::mutate_(.dots = stats::setNames(list(~dplyr::row_number(split_by)), replicate_by)) %>%
        dplyr::ungroup()

      strata <- c(strata, replicate_by)
    }


    foreach::foreach(variable = variables, .combine = rbind) %dopar%
    {

      sample %>%
        split(.[split_by]) %>%
        purrr::map_df(
            function() {
              correlation_matrix <-
                sample %>%
                dplyr::arrange_(.dots = strata) %>%
                dplyr::select_(.dots = c(strata, variable)) %>%
                tidyr::spread_(replicate_by, variable) %>%
                dplyr::select_(~-dplyr::one_of(setdiff(strata, replicate_by))) %>%
                stats::cor()
              median(correlation_matrix[upper.tri(correlation_matrix)])
            }) %>%
        dplyr::mutate(variable = variable)
    } %>%
      tidyr::gather_(replicate_by, "pearson", setdiff(names(.), "variable")) %>%
      dplyr::group_by_(.dots = c("variable")) %>%
      dplyr::summarize_at("pearson", dplyr::funs(median, min, max))
  }
