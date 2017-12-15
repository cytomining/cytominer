#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>

using namespace Rcpp;
using namespace std;
using namespace arma;


//' singular_value_entropy
//'
//' Calculate the entropy of a matrix singular values (SV) in the SVD decomposition, when the SVs summation is normalized to one.
//' @param A a matrix of any arbitrary size
//' @return entropy of the normalized singular values using log base 10
//'
//' @export
//'
// [[Rcpp::export]]
double singular_value_entropy(arma::mat A) {

  // calculate the svd
  vec singular_values = svd(A);

  // normalize relative values
  vec singular_values_nrm = singular_values/sum(singular_values);

  // calculate the entropy for values greater than 0 (to avoid log(0))
  arma::vec sv_nonzero = singular_values_nrm.elem(find(singular_values_nrm > 0));
  double sv_entropy = -sum(sv_nonzero % log(sv_nonzero))/log(10);

  return sv_entropy;
}

//' score_features_sv_entropy
//'
//' Scores each feature based on the difference of normalized SVs entropy of the data
//' with and without the feature; the higher the difference, the more informative the
//' feature would be.
//' @param data a matrix which represents the dataset; columns and rows correspond to features and observations, respectively.
//' @return vector containing scores for all the features, in the same order as the columns of data are arranged
//'
//' @export
//'
// [[Rcpp::export]]
NumericVector score_features_sv_entropy(NumericMatrix data){

  // convert into matrix (armadillo)
  mat data_mat(data.begin(), data.nrow(), data.ncol(), false);

  // total entropy
  double sv_entropy_orig = singular_value_entropy(data_mat);

  // vector of contribution to the entropy by on a leave-a-feature-out basis
  NumericVector sv_entropy(data.nrow());

  // for each feature calculate the contribution to the entropy by leaving that feature out
  for(unsigned int i = 0; i < data.nrow(); i++){

    mat data_mat_i = data_mat;

    // remove the row and column i
    data_mat_i.shed_row(i);
    data_mat_i.shed_col(i);

    double sv_entropy_i = singular_value_entropy(data_mat_i);
    sv_entropy[i] = sv_entropy_orig - sv_entropy_i;
  }

  return(sv_entropy);
}
