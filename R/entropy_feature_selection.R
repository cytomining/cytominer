#' entropy based feature selection
#'
#' @param X named tbl containing data, where columns and rows correspond to features and samples, respectively. Column names are assumed to be feature names.
#' @param n_features number of features to be extracted
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom foreach %dopar%
#'
#' @return vector containing name of the selected features
#'
#' @export
entropy_feature_selection <- function(X, n_features) {

  A <- tcrossprod(t(X), t(X))

  feat_index <- CE_entropy_FS2_new(A, n_features)

  return(colnames(X)[feat_index[1:n_features]])
}
