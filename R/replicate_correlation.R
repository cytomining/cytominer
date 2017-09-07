utils::globalVariables(c("n", ".", "variable"))
#' Measure replicate correlation of variables.
#' 
#' `replicate_correlation` measures replicate correlation of variables.
#'
#' @param sample tbl containing sample used to estimate parameters.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for grouping prior to normalization.
#' @param replicates number of replicates. 
#' @param replicate_by optional character string specifying column containing the replicate id.
#' @param split_by optional character string specifying column  by which to split the sample into batches; replicate correlations will be calculate per batch.
#' @param cores optional integer specifying number of CPU cores used for parallel computing using \code{doParallel}.
#'
#' @return data frame of variable quality measurements
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom foreach %dopar%
#' @importFrom stats median
#'
#' @export
#'
replicate_correlation <-
  function(sample, variables, strata, replicates,
           replicate_by = NULL,
           split_by = NULL,
           cores = NULL) {

    doParallel::registerDoParallel(cores = cores)

    if (is.null(split_by)) {
      sample %<>% dplyr::mutate(col_split_by = 0)

      split_by <- "col_split_by"
    }

    if (is.null(replicate_by)) {
      replicate_by <- "col_replicate_by"

      sample %<>%
        dplyr::count_(vars = strata) %>%
        dplyr::filter(n == replicates) %>%
        dplyr::inner_join(sample) %>%
        dplyr::group_by_(.dots = strata) %>%
        dplyr::mutate(col_replicate_by = dplyr::row_number(n)) %>%
        dplyr::select(-n) %>%
        dplyr::ungroup()

      strata <- c(strata, replicate_by)
    }

    foreach::foreach(variable = variables, .combine = rbind) %dopar%
    {

      sample %>%
        split(.[split_by]) %>%
        purrr::map_df(
            function(sample_split) {
              correlation_matrix <-
                sample_split %>%
                dplyr::arrange_(.dots = strata) %>%
                dplyr::select_(.dots = c(strata, variable, replicate_by)) %>%
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
