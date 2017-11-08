#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>

using namespace Rcpp;
using namespace std;
using namespace arma;


//' calculate_entropy_new
//'
//' Calculate the entropy of a matrix singular values, when their summation is normalized to one.
//' @param A matrix of any arbitrary size
// entropy
//' @return Calculate the entropy contribution of a matrix based on SVD
// [[Rcpp::export]]
double calculate_sv_entropy(arma::mat A) {

  // calculate the svd
  vec s = svd(A);

  // normalize relative values
  vec v = s/sum(s);

  // calculate the entropy for values greater than 0 (to avoid error)
  arma::vec vNoZero = v.elem(find(v > 0));
  double e = -sum(vNoZero % log(vNoZero))/log(10);

  return e;
}

/* SR method
Simple Ranking:
select mc features according to the highest ranking order of their CE values
*/

//' CE_entropy_SR
//'
//' Calculate the entropy contribution of a matrix based on SVD
//' @param A dataset mxn (m features and n observations)
//' @return entropy contribution vector
// [[Rcpp::export]]
NumericVector CE_entropy_SR(NumericMatrix A){

  // convert into matrix (armadillo)
  mat Amat(A.begin(), A.nrow(), A.ncol(), false);

  // total entropy
  double E = calculate_sv_entropy(Amat);

  // vector of contribution to the entropy by on a leave-a-feature-out basis
  NumericVector CE(A.nrow());

  // vector of selected feature indices
  NumericVector selected_indx;

  // for each feature calculate the contribution to the entropy by leaving that feature out
  for(unsigned int i = 0; i < A.nrow(); i++){

    mat Ai = Amat;

    // remove the row and column i
    Ai.shed_row(i);
    Ai.shed_col(i);

    double Ei = calculate_sv_entropy(Ai);
    CE[i] = E - Ei;
  }

  return(CE);
}
