utils::globalVariables(c(".", "i"))
#' Feature importance based on data entropy.
#'
#' \code{svd_entropy} measures the contribution of each feature in decreasing the data entropy.
#'
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample used to estimate parameters.
#' @param cores optional integer specifying number of CPU cores used for parallel computing using \code{doParallel}.
#'
#' @return data frame specifying the contribution of each feature in decreasing the data entropy.
#' Higher values indicate more information.
#'
#' @importFrom foreach %dopar%
#' @importFrom magrittr %>%
#'
#' @examples
#' sample <- tibble::tibble(
#'   AreaShape_MinorAxisLength = c(10, 12, 15, 16, 8, 8, 7, 7, 13, 18),
#'   AreaShape_MajorAxisLength = c(35, 18, 22, 16, 9, 20, 11, 15, 18, 42),
#'   AreaShape_Area = c(245, 151, 231, 179, 50, 112, 53, 73, 164, 529)
#' )
#' variables <- c("AreaShape_MinorAxisLength", "AreaShape_MajorAxisLength", "AreaShape_Area")
#' svd_entropy(variables, sample, cores = 1)
#' @export
svd_entropy <- function(variables, sample, cores = NULL) {
  doParallel::registerDoParallel(cores = cores)

  sample %<>%
    dplyr::select(variables) %>%
    dplyr::collect()

  # to ensure the ordering is captured
  variables <- colnames(sample)

  entropy_scores <-
    as.matrix(sample) %>%
    crossprod(., .) %>%
    entropy_score()

  doParallel::stopImplicitCluster()

  dplyr::tibble(
    variable = variables,
    svd_entropy = entropy_scores
  )
}

singular_value_entropy <- function(A) {
  singular_values <- svd(A, 0, 0)$d

  # normalize
  singular_values <- singular_values / sum(singular_values)

  # entropy
  -sum(singular_values * log10(singular_values))
}

entropy_score <- function(data) {

  # calculate contribution of each features to the entropy by leaving that feature out
  sv_entropy <-
    foreach::foreach(i = 1:ncol(data), .combine = c) %dopar% singular_value_entropy(data[-i, -i])

  singular_value_entropy(data) - sv_entropy
}

