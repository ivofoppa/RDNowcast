#include <Rcpp.h>
using namespace Rcpp;

//[Rcpp::interfaces(cpp)]

//' Generates data for posterior probabilities for Nowcasting
//'  
//' @description
//' This function generates the numbers used for data completeness estimation and generates posterior completeness probabilities for Nowcasting full numbers.
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
//' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
//' @returns A list with two integer arrays of dimensions \code{NCperiod} \eqn{\times} NCsize; the first with the reported, the second with the full numbers. 
//' @example man/examples/NowcastProb.R
//' 
//' @export
//' 
// [[Rcpp::export]]
NumericMatrix NowcastProb(DataFrame data, DateVector NCdates, int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          int week_start = 2, String unit = "week",int nsamples = 100000) {
  
  DateVector evdates = data[reference_date];
  DateVector rpdates = data[report_date];
  DateVector NCdatessrt = rev(sort_unique(NCdates));
  double alpha;
  double beta;
  
  Date dte, NCstart = Date(NCdatessrt[0]);
  int mult, dtecomp, NCperiod = NCdatessrt.length(), day_anal = NCstart.getWeekday(),
    nsamplesprop = ceil(nsamples/NCperiod);
  
  if (unit=="week") {
    dtecomp = day_anal - week_start;
  } else {
    dtecomp = 0;
  }
  
  IntegerMatrix obs(NCperiod,NCsize);
  IntegerMatrix full(NCperiod,NCsize);
  
  if (unit=="day") {
    mult = 1;
  } else {
    mult = 7;
  }
  int k = 0;
  for(DateVector::iterator d = NCdatessrt.begin(); d != NCdatessrt.end(); ++d) {
    
    IntegerVector countVec(NCsize), countVecRep(NCsize);
    
    for(int i=0; i<NCsize; i++) {
      dte = Date(*d) + -((i + 1)*mult);
      
      DateVector repdatessub(evdates[ (evdates>=(dte+ -dtecomp)) & (evdates< (dte + mult + -dtecomp)) & (rpdates < Date(*d)) ]);
      DateVector evdatessub(evdates[ (evdates>=(dte+ -dtecomp)) & (evdates< (dte + mult + -dtecomp))]);
      
      countVec.push_front(evdatessub.length());
      countVecRep.push_front(repdatessub.length());
    }
    
    obs(k, _ ) = countVecRep;
    full(k, _ ) = countVec;
    ++k;
  }
  
  int nrows = obs.nrow(), ncols = obs.ncol();
  NumericVector pvec(nsamplesprop*nrows);
  NumericMatrix pmat(nsamplesprop*nrows,ncols);
  
  for(int i=0; i<ncols; i++) {
    for(int k=0; k<nrows; k++) {
      alpha = obs(k,i) + 1;
      beta = full(k,i) - obs(k,i) + 1;
      for(int l=0; l<nsamplesprop; l++) {
        pvec[k*nsamplesprop + l] = R::rbeta(alpha,beta);
      } 
    }
    pmat(_,i) = pvec;
  }
  // Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
  return pmat;
}
