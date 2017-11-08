#' entropy based feature selection
#'
#' @param population named tbl containing data, where columns and rows correspond to features and samples, respectively. Column names are assumed to be feature names.
#' @param variables vector containing the names of numerical variables (or features) in the population.
#' @param n_feature integer specifying number of features to be selected
#'
#' @importFrom magrittr %>%
#'
#' @return vector containing name of the selected features
#'
#' @export
entropy_feature_selection <- function(population, variables, n_feature) {

  population_data <- population %>%
    dplyr::select(dplyr::one_of(variables)) %>%
    as.matrix()

  feat_inner_prods <- crossprod(population_data, population_data)

  CE <- CE_entropy_SR(feat_inner_prods)

  feat_rank <- order(CE, decreasing = T)

  return(list(feats = colnames(population_data)[feat_rank[1:n_feature]],
              CE = CE[feat_rank[1:n_feature]]))
}
