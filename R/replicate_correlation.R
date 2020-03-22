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
#' @examples
#' set.seed(123)
#' x1 <- rnorm(10)
#' x2 <- x1 + rnorm(10) / 100
#' y1 <- rnorm(10)
#' y2 <- y1 + rnorm(10) / 10
#' z1 <- rnorm(10)
#' z2 <- z1 + rnorm(10) / 1
#'
#' batch <- rep(rep(1:2, each = 5), 2)
#'
#' treatment <- rep(1:10, 2)
#'
#' replicate_id <- rep(1:2, each = 10)
#'
#' sample <-
#'   tibble::tibble(
#'     x = c(x1, x2), y = c(y1, y2), z = c(z1, z2),
#'     Metadata_treatment = treatment,
#'     Metadata_replicate_id = replicate_id,
#'     Metadata_batch = batch
#'   )
#'
#' head(sample)
#'
#' # `replicate_correlation`` returns the median, min, and max
#' # replicate correlation (across batches) per variable
#' replicate_correlation(
#'   sample = sample,
#'   variables = c("x", "y", "z"),
#'   strata = c("Metadata_treatment"),
#'   replicates = 2,
#'   split_by = "Metadata_batch",
#'   replicate_by = "Metadata_replicate_id",
#'   cores = 1
#' )
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

    .strata <- rlang::syms(strata)

    if (is.null(split_by)) {
      sample %<>% dplyr::mutate(col_split_by = 0)

      split_by <- "col_split_by"
    }

    if (is.null(replicate_by)) {
      replicate_by <- "col_replicate_by"

      sample %<>%
        dplyr::count(!!!.strata) %>%
        dplyr::filter(n == replicates) %>%
        dplyr::inner_join(sample) %>%
        dplyr::group_by(!!!.strata) %>%
        dplyr::mutate(col_replicate_by = dplyr::row_number(n)) %>%
        dplyr::select(-n) %>%
        dplyr::ungroup()

      strata <- c(strata, replicate_by)

      .strata <- rlang::syms(strata)
    }

    result <-
      foreach::foreach(variable = variables, .combine = rbind) %dopar%
      {
        sample %>%
          split(.[split_by]) %>%
          purrr::map_df(
            function(sample_split) {
              strata_no_replicate_by <- setdiff(strata, replicate_by)

              correlation_matrix <-
                sample_split %>%
                dplyr::arrange(!!!.strata) %>%
                dplyr::select(c(strata, variable, replicate_by)) %>%
                tidyr::spread_(replicate_by, variable) %>%
                dplyr::select(-strata_no_replicate_by) %>%
                stats::cor()
              median(correlation_matrix[upper.tri(correlation_matrix)])
            }
          ) %>%
          dplyr::mutate(variable = variable)
      } %>%
      tidyr::gather_(replicate_by, "pearson", setdiff(names(.), "variable")) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarize_at("pearson", c("median", "min", "max"))

    doParallel::stopImplicitCluster()

    result
  }
