#include <Rcpp.h>
using namespace Rcpp;

//' Generates data for nowcasting
//'  
//' @description
//' This function generates the numbers used for data completeness estimation, used for nowcasting full numbers
//' 
//' @param df A data frame containing data, with one column representing event times and one with reporting times.
//'     The data should be complete in the sense, that reporting delays no longer play a role. 
//' @param NCstart A date in the format "YYYY-mm-dd" representing the most recent data for data completeness estimation.
//' @param NCsize An integer, representing the length of the time series in chosen time units, for which completeness estimates should be obtained.
//' @param NCperiod The numbers of weeks used for data generation. By default 52 weeks.
//' @param reference_date A string variable representing the name of the column with event dates.
//' @param report_date A string variable representing the name of the column with reporting dates.
//' @param day_anal Integer representing the day of the analysis, with 1 (sunday) to 7 (saturday); the most recent reporting date is assumed
//'     to be on day_anal - 1.
//' @param week_start Integer representing the start of the week; default (monday: 2).
//' @param unit String representing time unit of analysis, week ("week") vs. day ("day").
//' 
//' @returns A list with two integer arrays of dimensions NCperiod $\times$ NCsize; the first with the reported, the second with the full numbers. 
//' 
//' @export
//' 
// [[Rcpp::export]]
 List NowcastDataMat(DataFrame df, Date NCstart, int NCsize = 10, int NCperiod = 52, 
                    String reference_date = "reference_date", String report_date = "report_date", 
                    int day_anal = 5, int week_start = 2, String unit = "week") {
  
  DateVector evdates = df[reference_date];
  DateVector rpdates = df[report_date];
  
  Date dte, NCstart_new = NCstart;
  int dtecomp = day_anal - week_start;
  int mult;
  
  IntegerMatrix obs(NCperiod,NCsize);
  IntegerMatrix full(NCperiod,NCsize);

  if (unit=="day") {
    mult = 1;
  } else {
    mult = 7;
  }
  
  for(int k=0; k<NCperiod; k++) {
    NCstart_new = NCstart + -k*7;
    
    while(NCstart_new.getWeekday()!=week_start) {
      NCstart_new = NCstart_new + -1;
    }
    
    IntegerVector countVec(NCsize), countVecRep(NCsize);

    for(int i=0; i<NCsize; i++) {
      dte = NCstart_new + -i*mult;
      
      DateVector repdatessub(evdates[ (evdates>=dte) & (evdates< dte + mult) & (rpdates < NCstart_new + mult + dtecomp) ]);
      DateVector evdatessub(evdates[ (evdates>=dte) & (evdates< dte + mult )]);
      
      countVec.push_front(evdatessub.length());
      countVecRep.push_front(repdatessub.length());
    }
    
    obs(k, _ ) = countVecRep;
    full(k, _ ) = countVec;
  }
  
  List L = List::create(obs, full);
  
  return L;
}
//' 
//' @description
//' This function generates estimates for the data completeness that can be used to generate Nowcast estimates. Specifically, the two required
//' inputs are raws samples from the posterior distribution of the proportion \code{p}, given \code{x} ``successes'' in \code{N} trials. 
//'     It makes use of the fact, that the distribution is proportional to the Gamma distribution with parameters \code{\eqn{x + 1}} and
//'     \code{\eqn{N - x + 1}}, where both parameters of the Gamma prior are chosen to be 1 (vague prior).
//' 
//' @param x An integer vector with currently observed numbers.
//' @param N An integer vector with n ("complete") numbers.
//' @param nsamples The number of samples; the default is 10'000.
//' @returns A numeric vector.
//' @examples
//' pSample(c(10,20),c(12,40))
//' pSample(23,100)
//' @export
//' 
// [[Rcpp::export]]
NumericMatrix pSample(IntegerVector x, IntegerVector n, int nsamples = 10000){
  
  Function f("rbeta");
  int vlen = x.length();
  int inputCheck = 0;
  NumericVector alpha(vlen);
  NumericVector beta(vlen);
  NumericVector pvec(nsamples);
  NumericMatrix pmat(nsamples,vlen);
  
  if (vlen != n.length())
    Rcpp::stop("The two input vectors have to be of the same size!");
  
  for (int k=0; k<vlen; k++) {
    if(x[k]>n[k]) {
      inputCheck=+1;
    }
  }
  
  if (inputCheck > 0)
    Rcpp::stop("the number of ""successes"" (elements of first input vector) cannot exceed the total number (second vector)!");
  
  for(int k=0; k<vlen; k++) {
    alpha[k] = x[k] + 1;
    beta[k] = n[k] - x[k] + 1;
    pvec = f(nsamples, Named("shape1",alpha[k]), Named("shape2",beta[k]));
    
    for(int j=0; j<nsamples; j++) {
      pmat(j,k) = pvec[j];
    }
  }
  
 // Next code is interpreted as rbeta(n=10, shape1=alpha, shape2=beta)
  return pmat;
}

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
  
  Function f("rbeta");
  
  IntegerMatrix x = NC[0], n =NC[1];
  int xrows = x.nrow();
  int xcols = x.ncol();
  int inputCheck = 0;
  double alpha;
  double beta;
  NumericVector pvecone(nsamples);
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
      pvecone = f(nsamples, Named("shape1",alpha), Named("shape2",beta));
      
      for(int j=0; j<nsamples; j++) {
        pvec(k*nsamples + j) = pvecone[j];
      }
    }
    pmat(_,i) = pvec;
  }
 // Next code is interpreted as rbeta(n=10, shape1=alpha, shape2=beta)
  return pmat;
}
