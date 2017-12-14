#' feature selection based on the entropy of data singular values
#'
#' @param population named tbl containing data, where columns and rows correspond to features (and/or metadata) and samples, respectively. Column names are assumed to be feature or metadata names.
#' @param variables vector containing the names of numerical variables (or features) in the population.
#' @param n_feature integer specifying number of features to be selected
#'
#' @importFrom magrittr %>%
#'
#' @return vector containing name of the features sorted based on their score, and the actual score values. Higher score means more informative feature.
#'
#' @export
entropy_feature_selection <- function(population, variables, n_feature) {

  population_data <- population %>%
    dplyr::select(dplyr::one_of(variables)) %>%
    as.matrix()

  # working with the matrix inner product; as it would be computationally more efficient for large number of samples
  feat_inner_prods <- crossprod(population_data, population_data)

  entropy_score <- score_features_sv_entropy(feat_inner_prods)

  feat_rank <- order(entropy_score, decreasing = T)

  return(list(feats = colnames(population_data)[feat_rank[1:n_feature]],
              entropy_score = entropy_score[feat_rank[1:n_feature]]))
}
