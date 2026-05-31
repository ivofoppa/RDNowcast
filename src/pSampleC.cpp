#include <Rcpp.h>
using namespace Rcpp;

//' Generating posterior distribution of data completeness estimates
//' 
//' @description 
//' This function generates builds on the function \code{pSample}, but if the data completeness estimates are obtained for more than one data set.
//'     The inputs are arrays for the number of successes (x) and totals (n). Each entry in x should correspond to an entry in n and the rows typically
//'     represent a given temporal distance to the "present" (time of the nowcast). The samples correspondig
//' 
//' @param NC A list whose first element is an integer array with observed numbers (incomplete due to reporting delay); 
//'     the columns represent decreasing temporal distance from "now", the rows represent the complete numbers.
//'     different times in the nowcast period. 
//' @param nsamples The number of samples; the default is 10'000.
//' @returns A numeric vector.
//' @examples
//' pSample(c(10,20),c(12,40))
//' pSample(23,100)
//' 
//' @export
// [[Rcpp::export]]
 NumericMatrix pSampleMult(List NC, int nsamples = 10000){
   
   IntegerMatrix x = NC[0], n =NC[1];
   int xrows = x.nrow();
   int xcols = x.ncol();
   int inputCheck = 0;
   double alpha;
   double beta;
   NumericVector pvec(nsamples*xrows);
   NumericMatrix pmat(nsamples*xrows,xcols);
   
   if (xrows != n.nrow() || xcols != n.ncol())
     Rcpp::stop("The two input matrices have to be of the same dimensions!");
   
   for (int k=0; k<xrows; k++) {
     for(int l=0; l<xcols; l++) {
       if(x(k,l) > n(k,l)) {
         inputCheck++;
       }
     }
   }
   if (inputCheck > 0)
     Rcpp::stop("the number of ""successes"" (elements of first input matrix) cannot exceed the total number (second input matrix)!");
   
   for(int i=0; i<xcols; i++) {
     for(int k=0; k<xrows; k++) {
       alpha = x(k,i) + 1;
       beta = n(k,i) - x(k,i) + 1;
       for(int l=0; l<nsamples; l++) {
         pvec[k*nsamples + l] = R::rbeta(alpha,beta);
       } 
     }
     pmat(_,i) = pvec;
   }
  // Next code is interpreted as rbeta(n=10, shape1=alpha, shape2=beta)
   return pmat;
 }
 