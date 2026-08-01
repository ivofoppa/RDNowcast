#include <Rcpp.h>
#include "funN.h"
#include "NowcastProb.h"
using namespace Rcpp;

//' Generates data for posterior probabilities for Nowcasting
//'  
//' @description
//' This function generates the numbers used for data completeness estimation and generates posterior completeness probabilities for Nowcasting full numbers. The data used for this are reconstructed from historical data
//'     for which reporting delay no longer has a relevant effect. 
//' 
//' @param data A data frame containing data, with one column representing event times and one with reporting times.
//'     The data should be complete in the sense, that reporting delays no longer play a role. 
//' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
//' @param offset The number of days before dateAnal of the most recent reference date considered.
//' @param NCdates A date vector, representing the dates used for the completeness estimation; these dates have to correspond to the same day of week as dateAnal.
//' @param NCsize An integer, representing the length of the time series in chosen time units, for which completeness estimates should be obtained.
//' @param NCperiod The numbers of weeks (data sets) used for completeness estimation. By default 52 weeks. A data set consists of observed counts for a time series of length \code{NCsize} time units.
//'     These count have the same temporal relationship to the hypothetical time of analysis we have no to the data to be nowcasted. The second component is the complete data.
//'     These numbers can be generated from a data for which reporting delay no longer has a relevant effect. 
//' @param reference_date A string variable representing the name of the column with event dates.
//' @param report_date A string variable representing the name of the column with reporting dates.
//' @param unit String representing time unit of analysis, week ("week") vs. day ("day").
//' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
//' @returns A list with two integer arrays of dimensions \code{NCperiod} \eqn{\times} NCsize; the first with the reported, the second with the full numbers. 
// //' @example man/examples/Nowcast_example.R
//' 
//' @export
//' 
// [[Rcpp::export]]
IntegerMatrix Nowcast(DataFrame data, Date dateAnal, int offset, DateVector NCdates, int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          String unit = "week", int nsamples = 100000) {
  
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
int mult;

if (unit=="day") {
  mult = 1;
} else {
  mult = 7;
}

for(int i=0;i<NCsize;i++) {
  analdates[i] = Date(dateAnal + -offset + -mult*(i + 1));
}
analdates = rev(analdates);

for(DateVector::iterator d = analdates.begin(); d != analdates.end(); ++d) {
  dte = Date(*d);
  
  DateVector repdatessub(evdates[ (evdates>dte) & (evdates<= Date(dte + mult)) & (rpdates < Date(dateAnal)) ]);
  countVec.push_back(repdatessub.length());
}

//NumericMatrix pmat = NowcastProb(data = data, NCdates = NCdates, offset = offset, NCsize = NCsize, reference_date = reference_date, 
//                                 report_date = report_date, unit = unit,nsamples = nsamples);
NumericMatrix pmat = NowcastProb(data, NCdates, offset, NCsize, reference_date, report_date, unit, nsamples);
IntegerMatrix N_full = f_N_arr(countVec, pmat);
// Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
return N_full;
}
