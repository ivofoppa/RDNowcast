#include <Rcpp.h>
#include "funN.h"
using namespace Rcpp;
//[Rcpp::interfaces(cpp)]
//' Generates data for posterior probabilities for Nowcasting
//'  
//' @description
//' This function generates the numbers used for data completeness estimation and generates posterior completeness probabilities for Nowcasting full numbers.
//' 
//' @param data A data frame containing data, with one column representing event times and one with reporting times.
//'     The data should be complete in the sense, that reporting delays no longer play a role. 
//' @param NCdates A date vector of the historical dates of the same day of week as the date of analysis, for which the data can be assumed to be complete. 
//' @param offset An integer, the number of days before the date of analysis the most recent events are to be considered. 
//' @param week_start An integer, denoting start of the week (1: Sunday); default value is 2 (Monday).
//' @param NCsize An integer, representing the length of the time series in chosen time units, for which completeness estimates should be obtained.
//' @param reference_date A string variable representing the name of the column with event dates.
//' @param report_date A string variable representing the name of the column with reporting dates.
//' @param unit String representing time unit of analysis, week ("week") vs. day ("day").
//' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
//' @returns A numerical array, each column representing posterior samples of the completeness distribution for a particular nowcast value. The last column represents the most recent number to be nowcasted. 
//' @example man/examples/NowcastFull_example.R
//' 
//' @export
//' 
// [[Rcpp::export]]
IntegerMatrix NowcastFull(DataFrame data, Date dateAnal, Date recentRef, DateVector NCdates, int week_start = 2, int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          String unit = "week",int nsamples = 100000) {
  
  DateVector evdates = data[reference_date];
  DateVector rpdates = data[report_date];
  DateVector NCdatessrt = rev(sort_unique(NCdates));
  NCdatessrt.push_back(dateAnal);

  double alpha;
  double beta;
  Date dte, dtefrom, dteto1, dteto2, weekstrt = recentRef;
  int mult, NCperiod = NCdatessrt.length(),
    offset = dateAnal - recentRef;
  DateVector NCdatesfrom, NCdatesto;
  
  IntegerMatrix obs(NCperiod,NCsize), obs2(NCperiod - 1,NCsize);
  IntegerMatrix full(NCperiod,NCsize), full2(NCperiod - 1,NCsize);
  
  
  if (unit=="day") {
    mult = 1;
  } else {
    mult = 7;
  }
  
  while(weekstrt.getWeekday()!=week_start) {
    weekstrt = Date(weekstrt + -1);
  }
  
  int offset2 = dateAnal - weekstrt;
  
//offset2 = Date(NCdatessrt[0]) - Date(weekstrt);
  
  for(DateVector::iterator d = NCdatessrt.begin(); d != NCdatessrt.end(); ++d) {
    
    NCdatesfrom.push_back(Date(*d + -offset2)); // start of previous week
    NCdatesto.push_back(Date(*d + -offset));
  }
  
  int k = 0;
  for(DateVector::iterator d = NCdatessrt.begin(); d != NCdatessrt.end(); ++d) {
    
    IntegerVector countVec, countVecRep;
    dte = Date(*d);
    dteto1 = Date(NCdatesto[k]);
    
    for(int i=0; i<NCsize; i++) {
      dtefrom = Date(NCdatesfrom[k] + -(i*mult));
      dteto2 = Date(NCdatesfrom[k] + -(i*mult) + mult + -1);
      
      if(i==0 && (dteto2 > dteto1) && unit=="week") {
        DateVector repdatessub(evdates[ (evdates>=dtefrom) & (evdates<=dteto1) & (rpdates < dte) ]);
        DateVector evdatessub(evdates[ (evdates>=dtefrom) & (evdates<=dteto2)]);
        countVec.push_front(evdatessub.length());
        countVecRep.push_front(repdatessub.length());
      } else {
        DateVector repdatessub(evdates[ (evdates>=dtefrom) & (evdates<=dteto2) & (rpdates < dte) ]);
        DateVector evdatessub(evdates[ (evdates>=dtefrom) & (evdates<=dteto2)]);
        countVec.push_front(evdatessub.length());
        countVecRep.push_front(repdatessub.length());
      }
    }
    
    obs(k, _ ) = countVecRep;
    full(k, _ ) = countVec;
    ++k;
  }
  
  IntegerVector obsVec = obs(0, _ );
  
  for (int k=1; k<NCperiod; k++) {
    obs2(k-1, _ ) = IntegerVector(obs(k, _ ));
    full2(k-1, _ ) = IntegerVector(full(k, _ ));
  }
  
  int nrows = obs2.nrow(), ncols = obs2.ncol();
  NumericVector pvec(nsamples*nrows);
  NumericMatrix pmat(nsamples*nrows,ncols);
  
  for(int i=0; i<ncols; i++) {
    for(int k=0; k<nrows; k++) {
      alpha = obs2(k,i) + 1;
      beta = full2(k,i) - obs2(k,i) + 1;
      for(int l=0; l<nsamples; l++) {
        pvec[k*nsamples + l] = R::rbeta(alpha,beta);
      } 
    }
    pmat(_,i) = pvec;
  }
  IntegerMatrix N_full = f_N_arr(obsVec, pmat);
  // Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
  return N_full;
}
 