#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>

using namespace Rcpp;
using namespace std;
using namespace arma;


//' calculate_entropy_new
//'
//' Calculate the entropy of a matrix based on SVD alternative method when more observations than features. A = XtX instead of A = Xt
//' @param A dataset mxm (m features and n observations)
// entropy
//' @return Calculate the entropy contribution of a matrix based on SVD
// [[Rcpp::export]]
double calculate_entropy_new(arma::mat A) {

  // calculate the svd
  vec s = svd(A);

  // normalize relative values
  vec v = s/sum(s);

  // calculate the entropy for values bigger than 0 (to avoid error)
  arma::vec vNoZero = v.elem(find(v > 0));
  double e = -sum(vNoZero % log(vNoZero))/log(v.size());

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
  double E = calculate_entropy_new(Amat);
  // Contributio vector to the entropy by a leave-one-out comparison
  NumericVector CE(A.nrow());
  // for each feature calculate the contribution to the entropy by a leave-one-out comparison
  for(unsigned int i = 0; i < A.nrow(); i++){
    //cout << "i: " << i << endl;
    mat Ai = Amat;
    // remove the row i
    Ai.shed_row(i) ;
    //cout << "Ai size: " << Ai.n_cols << "columns and " << Ai.n_rows << "rows" << endl;
    double Ei = calculate_entropy_new(Ai);
    CE[i] = E - Ei;
  }

  return CE;
}

//--------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------

/* FS1 method
* Forward selection
*/

/*
//' Calculate the entropy of a matrix based on SVD with FS method
//' @param A dataset mxn (m features and n observations)
//' @param mc number of feature to select
//' @return index of features that were selected
// [[Rcpp::export]]
NumericVector CE_entropy_FS1(NumericMatrix A, unsigned int mc){
// convert into matrix (armadillo)
mat Amat(A.begin(), A.nrow(), A.ncol(), false);

// vector of index of the features
vec idxFeat(linspace<vec>(1, Amat.n_rows, Amat.n_rows));

// best features index
NumericVector idxBest(mc);

// total entropy
double E = calculate_entropy(Amat);

// CE is the contribution vector of each feature to the entropy
vec CE(A.nrow());

// for each feature calculate the contribution to the entropy by a leave-one-out comparison
for(unsigned int i = 0; i < A.nrow(); i++){
mat Ai = Amat;
// remove the row i
Ai.shed_row(i);
//cout << "Ai size: " << Ai.n_cols << "columns and " << Ai.n_rows << "rows" << endl;
double Ei = calculate_entropy(Ai);
CE[i] = E - Ei;
}

// select the best feature
int idx = CE.index_max();

// index of the best feature in the R referential
idxBest[0] = idxFeat[idx];
// remove the selected feature number
idxFeat.shed_row(idx);

// save the previous entropy contribution
double Eprev = CE[idx];

// save selected features in a new matrix newA and remove it from the other matrix
mat newA(mc, Amat.n_cols, fill::zeros);
newA.row(0)  = Amat.row(idx);
Amat.shed_row(idx);

for(unsigned int j = 0; j < mc - 1; j++){
vec CE(Amat.n_rows);
// for all features
for(unsigned a = 0; a < Amat.n_rows; a++){
//bind the vector to Atest
mat Atest = join_cols(newA.rows(0, j), Amat.row(a));
// calculate the entropy on the features selected and an extra one
double Ei = calculate_entropy(Atest);
// gain of entropy
CE[a] = Ei - Eprev;
}
// index of best entropy contribution
int idx = CE.index_max();
idxBest[j+1] = idxFeat[idx];

// update the previous entropy contribution
Eprev = CE[idx];
newA.row(j+1) = Amat.row(idx);
Amat.shed_row(idx);
idxFeat.shed_row(idx);
}

cout << "Best features: " << idxBest << endl;

return idxBest;
}*/

/*
//' Calculate the entropy of a matrix based on SVD with FS method and new SV-entropy function
//' @param A dataset mxm (m features and n observations) m << n
//' @param mc number of feature to select
//' @return index of features that were selected
// [[Rcpp::export]]
NumericVector CE_entropy_FS1_new(NumericMatrix A, unsigned int mc){
// convert into matrix (armadillo)
mat Amat(A.begin(), A.nrow(), A.ncol(), false);

// vector of index of the features
vec idxFeat(linspace<vec>(1, Amat.n_rows, Amat.n_rows));

// best features index
NumericVector idxBest(mc);

// total entropy
double E = calculate_entropy_new(Amat);

// CE is the contribution vector of each feature to the entropy
vec CE(A.nrow());

// for each feature calculate the contribution to the entropy by a leave-one-out comparison
for(unsigned int i = 0; i < A.nrow(); i++){
mat Ai = Amat;
// remove the row i and column i
Ai.shed_row(i);
Ai.shed_col(i);
//cout << "Ai size: " << Ai.n_cols << "columns and " << Ai.n_rows << "rows" << endl;
double Ei = calculate_entropy_new(Ai);
CE[i] = E - Ei;
}

// select the best feature
int idx = CE.index_max();

// index of the best feature in the R referential
idxBest[0] = idxFeat[idx];
// remove the selected feature number
idxFeat.shed_row(idx);

// save the previous entropy contribution
double Eprev = CE[idx];

// save selected features in a new matrix newA and remove it from the other matrix
// PROBLEM HERE!!!!!!

mat newA(mc, Amat.n_cols, fill::zeros);
newA.row(0)  = Amat.row(idx);
Amat.shed_row(idx);

for(unsigned int j = 0; j < mc - 1; j++){
vec CE(Amat.n_rows);
// for all features
for(unsigned a = 0; a < Amat.n_rows; a++){
//bind the vector to Atest
mat Atest = join_cols(newA.rows(0, j), Amat.row(a));
// calculate the entropy on the features selected and an extra one
double Ei = calculate_entropy_new(Atest);
// gain of entropy
CE[a] = Ei - Eprev;
}
// index of best entropy contribution
int idx = CE.index_max();
idxBest[j+1] = idxFeat[idx];

// update the previous entropy contribution
Eprev = CE[idx];
newA.row(j+1) = Amat.row(idx);
Amat.shed_row(idx);
idxFeat.shed_row(idx);
}

cout << "Best features: " << idxBest << endl;

return idxBest;
}
*/

//--------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------------

/* FS2 method
Forward selection
*/

/*
//' Calculate the entropy of a matrix based on SVD with FS method
//' @param A dataset mxn (m features and n observations)
//' @param mc number of feature to select
//' @return index of features that were selected
// [[Rcpp::export]]
NumericVector CE_entropy_FS2(NumericMatrix A, unsigned int mc){
// convert into matrix (armadillo)
mat Amat(A.begin(), A.nrow(), A.ncol(), false);

// vector of index of the features
vec idxFeat(linspace<vec>(1, Amat.n_rows, Amat.n_rows));

// best features index
NumericVector idxBest(mc);

for(unsigned int j = 0; j < mc; j++){
// total entropy
double E = calculate_entropy(Amat);

// Contribution vector to the entropy by a leave-one-out comparison
vec CE(Amat.n_rows);

// for each feature calculate the contribution to the entropy by a leave-one-out comparison
for(unsigned int i = 0; i < Amat.n_rows; i++){
mat Ai = Amat;
// remove row i
Ai.shed_row(i);
double Ei = calculate_entropy(Ai);
CE[i] = E - Ei;
}
// find the index of the highest entropy contribution
int idx = CE.index_max();
idxBest[j] = idxFeat[idx];

// remove the best feature
Amat.shed_row(idx);
idxFeat.shed_row(idx);
}
cout << "Best features: " << idxBest << endl;

return idxBest;
}
*/

//' CE_entropy_FS2_new
//'
//' Calculate the entropy of a matrix based on SVD with FS method
//' @param A dataset mxn (m features and n observations)
//' @param mc number of feature to select
//' @return index of features that were selected
// [[Rcpp::export]]
NumericVector CE_entropy_FS2_new(NumericMatrix A, unsigned int mc){
  // convert into matrix (armadillo)
  mat Amat(A.begin(), A.nrow(), A.ncol(), false);

  // vector of index of the features
  vec idxFeat(linspace<vec>(1, Amat.n_rows, Amat.n_rows));

  // best features index
  NumericVector idxBest(mc);

  for(unsigned int j = 0; j < mc; j++){
    //cout << "j: " << j << endl;
    // total entropy
    double E = calculate_entropy_new(Amat);

    // Contribution vector to the entropy by a leave-one-out comparison
    vec CE(Amat.n_rows);

    // for each feature calculate the contribution to the entropy by a leave-one-out comparison
    for(unsigned int i = 0; i < Amat.n_rows; i++){

      mat Ai = Amat;
      // remove row i and column i
      Ai.shed_row(i);
      Ai.shed_col(i);
      double Ei = calculate_entropy_new(Ai);
      CE[i] = E - Ei;
    }
    // find the index of the highest entropy contribution
    int idx = CE.index_max();
    idxBest[j] = idxFeat[idx];

    // remove the best feature
    Amat.shed_row(idx);
    Amat.shed_col(idx);
    idxFeat.shed_row(idx);
  }
  //cout << "Best features: " << idxBest << endl;

  return idxBest;
}
