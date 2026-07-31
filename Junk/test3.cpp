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
Date Test3(DataFrame data, Date dateAnal = Date("1900-01-01"), DateVector NCdates = DateVector::create("1900-01-01"), int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          int week_start = 2, String unit = "week",int nsamples = 100000,int nweeksc = 20, 
                          int NClen = 53) {
  
  DateVector rpdates = data[report_date], evdates = data[reference_date],analdates(NCsize);
  //Date dte, dateAnal = max(rpdates) + 1;
  Date dte;
  
  if(dateAnal==Date("1900-01-01")) {
    dateAnal = Date(max(rpdates) + 1);
  }
  
// Next code is interpreted as rbeta(full=10, shape1=alpha, shape2=beta)
return dateAnal;
}
