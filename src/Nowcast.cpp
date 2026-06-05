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
//' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
//' @param NCdates A date vector, representing the dates used for the completeness estimation; these dates have to correspond to the same day of week as dateAnal.
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
IntegerMatrix Nowcast(DataFrame data, Date dateAnal, DateVector NCdates, int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          int week_start = 2, String unit = "week",int nsamples = 100000) {
  
  DateVector rpdates = data[report_date], evdates = data[reference_date],analdates(NCsize);
  //Date dte, dateAnal = max(rpdates) + 1;
  Date dte;
  
  if(dateAnal > Date(max(rpdates) + 1)) 
    Rcpp::stop("Date parameter cannot be larger than most recent report_date + 1!");
  
for(DateVector::iterator d=NCdates.begin(); d!=NCdates.end(); ++d) {
  if(Date(*d) > Date(max(rpdates) + 1))
    Rcpp::stop("None of the nowcasting times can be later that max. report_date + 1!");
  if(Date(*d).getWeekday() != dateAnal.getWeekday())
    Rcpp::stop("All nowcasting dates have to be the same day of week as dateAnal!");
}

IntegerVector countVec;
int dtecomp,day_anal = dateAnal.getWeekday(), mult;

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

NumericMatrix pmat = NowcastProb(data, NCdates, NCsize, reference_date, report_date, week_start, unit,nsamples);
IntegerMatrix N_full = f_N_arr(countVec, pmat);
// Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
return N_full;
}
