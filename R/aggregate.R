#' Aggregate data based on given grouping.
#'
#' \code{aggregate} aggregates data based on the specified aggregation method.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for aggregation.
#' @param operation optional character string specifying method for aggregation. This must be one of the strings \code{"mean"}, \code{"median"}, \code{"mean+sd"}, or \code{"cov"}.
#' @param ... optional arguments passed to aggregation operation : \code{random_projection} is an optional matrix of size (# features * (#features + 1)/2, k); if operation is set to \code{"cov"}, this matrix would be used to project the resulting cov profile along k directions specified in the columns.
#'
#' @return aggregated data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::data_frame(
#'    Metadata_group = c("control", "control", "control", "control",
#'                       "experiment", "experiment", "experiment", "experiment"),
#'    Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'    AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
#'  )
#' variables <- c("AreaShape_Area")
#' strata <- c("Metadata_group", "Metadata_batch")
#' aggregate(population, variables, strata, operation = "mean")
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
aggregate <- function(population, variables, strata, operation="mean", ...) {

  if (operation == "mean") {
    aggregating_function <- dplyr::funs(mean)
    feature_wise <- T
  } else if (operation == "median") {
    aggregating_function <- dplyr::funs(median)
    feature_wise <- T
  } else if (operation == "mean+sd") {
    aggregating_function <- c(dplyr::funs(mean), dplyr::funs(sd))
    feature_wise <- T
  } else if (operation == "cov") {
    feature_wise <- F
    aggregating_function <- function(subpopulation, variables, ...) {
      population_cov <- stats::cov(subpopulation[, variables])
      feat_pairs_names <- outer(variables, variables, function(var1, var2) paste(var1, "__", var2, sep = ""))
      cov_profile <- population_cov[lower.tri(population_cov, diag = T)]
      profile_feat_names <- feat_pairs_names[lower.tri(feat_pairs_names, diag = T)]
      cov_profile <- t(as.matrix(cov_profile))
      colnames(cov_profile) <- profile_feat_names 
      
      dots <- list(...)
      random_proj <- dots[["random_projection"]]
      if (!is.null(random_proj)) {
        cov_profile <- cov_profile %*% random_proj
      }
      
      cov_profile
    }
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  if (feature_wise) {
    outp <- population %>%
      dplyr::group_by_(.dots = strata) %>%
      dplyr::summarise_at(.funs = aggregating_function, .vars = variables) %>%
      dplyr::ungroup()
  } else {
    outp <- population %>%
      dplyr::collect() %>%
      dplyr::group_by_(.dots = strata) %>%
      dplyr::do(data.frame(aggregating_function(subpopulation = ., variables = variables, ...))) %>%
      dplyr::ungroup()
  }
  
  return(outp)
}
