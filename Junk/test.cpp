#include <Rcpp.h>
#include "funN.h"
#include "NowcastProb.h"
using namespace Rcpp;

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
//' 
//' @export
//' 
// [[Rcpp::export]]
IntegerMatrix Test(DataFrame data, String dateAnal = "1900-01-01",
                   CharacterVector NCdates = CharacterVector({"1900-01-01","1900-01-01"}),int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          int week_start = 2, String unit = "week",int nsamples = 100000) {
  
  DateVector rpdates = data[report_date], evdates = data[reference_date],analdates(NCsize),
    NCdatesProc;
//  Date dte, dateAnal = max(rpdates) + 1;
  Date dte, dateAnalProc;
  
  if(dateAnal=="19-01-01") {
    dateAnalProc = max(rpdates) + 1;
  } else {
    dateAnalProc = Date(dateAnal);
  }
  
  if(NCdates==CharacterVector({"1900-01-01","1900-01-01"})) {
    Rcpp::stop("At least one date has to be provided for the nowcast!");
  } else {
    for(CharacterVector::iterator d = NCdates.begin(); d != NCdates.end(); ++d) {
      dte = Date(*d);
      NCdatesProc.push_front(dte);
    }
  }
  
  IntegerVector countVec;
  int dtecomp,day_anal = dateAnalProc.getWeekday(),mult;
  
  if (unit=="week") {
    dtecomp = day_anal - week_start;
    mult = 7;
  } else {
    dtecomp = 0;
    mult = 1;
  }
  
  for(int i=0;i<NCsize;i++) {
    analdates[i] = Date(dateAnal+ -1 + -mult*i);
  }
  analdates = rev(analdates);
  
  for(int i=0; i<NCsize; i++) {
    dte = dateAnal + -((i + 1)*mult);
    
    DateVector repdatessub(evdates[ (evdates>=(dte+ -dtecomp)) & (evdates< (dte + mult + -dtecomp)) & (rpdates < dateAnal) ]);
    countVec.push_front(repdatessub.length());
  }

  NumericMatrix pmat = NowcastProb(data, NCdatesProc, NCsize, reference_date, report_date, week_start, unit,nsamples);
  IntegerMatrix N_full = f_N_arr(countVec, pmat);
  // Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
  return N_full;
}
