#include <Rcpp.h>
using namespace Rcpp;

//' Generates a vector of filled-in numbers 
//' 
//' \code{f_N_vec} returns a vector of random numbers from the distribution of the total number \code{N}, given the number of successes \code{x}
//'      and a vector of success probabilities, \code{p}. 
//' 
//' @param x Observed number 
//' @param pvec Vector of probabilities (data completeness)
//' @return Vector of same length as `pvec` of draws from the distribution of full numbers given the observed number \code{x} and the probabilities in `p`. 
//' @examples /man/examples/f_N_vec.R
//' @export
//' 
// [[Rcpp::export]]
IntegerVector f_N_vec(int x, NumericVector pvec) {
  
  if (min(pvec) < 0 || max(pvec)>1)
    Rcpp::stop("Elements of pvec have to be between 0 and 1!");
  
  if (x <= 0 )
    Rcpp::stop("x must be a positive integer!");
  
  const double tol = 1e-20;
  int stn = x,endn,plen = pvec.size();
  double pmin = min(pvec),pmax = max(pvec),p,sumprobs,rn;
  IntegerVector out(plen);
  
  if (R::dbinom(x, x, pmax, false) > tol) {
    stn = x;
  } else {
    stn = x + 1;
    while (R::dbinom(x, stn, pmax, false) < tol) {
      stn++;
    }
  }
  
  endn = stn + 1;
  while (R::dbinom(x, endn, pmax, false) > tol) {
    endn++;
  }
  
  while (R::dbinom(x, endn, pmin, false) > tol) {
    endn++;
  }
  
  int len = endn - stn + 1;
  
  IntegerVector nrng(len);
  for (int i = 0; i < len; i++) {
    nrng[i] = stn + i;
  }
  
  NumericVector probs(len);
  NumericVector probsnorm(len);
 
  for (int k = 0; k < plen; k++) {
    p = pvec[k];
    
    for (int i = 0; i < len; i++) {
      probs[i] = R::dbinom(x, nrng[i], p, false);
    }
    
    sumprobs = sum(probs);
    probsnorm = probs/sumprobs;
    NumericVector cum_probs = cumsum(probsnorm);
    
    rn = R::runif(0.0, 1.0);
    
    for (int l = 0; l < len; l++) {
      if (cum_probs[l] > rn) {
        out[k] = nrng[l];
        break;
      }
    }
  }  
  return out;
}

//' Generates an array of filled-in numbers 
//' 
//' @param x Vector of observed number 
//' @param pmat Array, with same number of columns as \code{x} has elements, of probabilities (data completeness)
//' @return Array of same dimensions as \code{pmat} of full numbers given the observed numbers \code{x} and the probabilities in \code{pmat}. 
//' @examples /man/examples/f_N_arr.R
//' @export
//' 
// [[Rcpp::export]]
IntegerMatrix f_N_arr(IntegerVector xvec, NumericMatrix pmat) {
  
  int xsze = xvec.size(),nsmpls = pmat.nrow();

  if (pmat.ncol() != xsze)
    Rcpp::stop("Dimensions of arguments are incompatible!");
  
  if (min(pmat) < 0 || max(pmat)>1)
    Rcpp::stop("Elements of pmat have to be between 0 and 1!");
  
  if (min(xvec) <= 0)
    Rcpp::stop("x must be positive!");
  
  int x;
  IntegerMatrix out(nsmpls,xsze);
  NumericVector pvec(nsmpls);
  
  for (int k=0; k<xsze; k++) {
    pvec = pmat( _ ,k);
    x = xvec[k];
    out( _ ,k) = f_N_vec(x,pvec);
  }  
  return out;
}


