#' Feature selection based on redundancy
#' \code{entropy_feature_selection} is a feature selection method based on the entropy of data singular values
#'
#' @param population named tbl containing data, where columns and rows correspond to features (and/or metadata) and samples, respectively. Column names are assumed to be feature or metadata names.
#' @param variables vector containing the names of numerical variables (or features) in the population.
#' @param n_feature integer specifying number of features to be selected
#'
#' @importFrom magrittr %>%
#'
#' @return vector containing name of the features sorted based on their score, and the actual score values. Higher score means more informative feature.
#'
#' @examples
#' population <- tibble::data_frame(
#'    AreaShape_MinorAxisLength = c(10, 12, 15, 16, 8, 8, 7, 7, 13, 18),
#'    AreaShape_MajorAxisLength = c(35, 18, 22, 16, 9, 20, 11, 15, 18, 42),
#'    AreaShape_Area = c(245, 151, 231, 179, 50, 112, 53, 73, 164, 529)
#'  )
#' variables <- c("AreaShape_MinorAxisLength", "AreaShape_MajorAxisLength", "AreaShape_Area")
#' entropy_feature_selection(population, variables, 2)
#'
#' @export
entropy_feature_selection <- function(population, variables, n_feature) {

  population_data <- population %>%
    dplyr::select(dplyr::one_of(variables)) %>%
    dplyr::collect() %>%
    as.matrix()


  # working with the matrix inner product; as it would be computationally more efficient for large number of samples
  feat_inner_prods <- crossprod(population_data, population_data)

  entropy_score <- score_features_sv_entropy(feat_inner_prods)

  feat_rank <- order(entropy_score, decreasing = T)

  return(list(features = colnames(population_data)[feat_rank[1:n_feature]],
              entropy_score = entropy_score[feat_rank[1:n_feature]]))
}
