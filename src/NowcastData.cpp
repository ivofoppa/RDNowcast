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
//' @returns A list with two integer arrays of dimensions \code{NCperiod} \eqn{\times} NCsize; the first with the reported, the second with the full numbers. 
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
   int mult, dtecomp;
   
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
   
   while(NCstart_new.getWeekday()!=day_anal) {
     NCstart_new = NCstart_new + -1;
   }
   
   for(int k=0; k<NCperiod; k++) {
     NCstart_new = NCstart + -k*7;
     
    IntegerVector countVec(NCsize), countVecRep(NCsize);
     
     for(int i=0; i<NCsize; i++) {
       dte = NCstart_new + -((i + 1)*mult);
       
       DateVector repdatessub(evdates[ (evdates>=(dte+ -dtecomp)) & (evdates< (dte + mult + -dtecomp)) & (rpdates < NCstart_new) ]);
       DateVector evdatessub(evdates[ (evdates>=(dte+ -dtecomp)) & (evdates< (dte + mult + -dtecomp))]);
       
       countVec.push_front(evdatessub.length());
       countVecRep.push_front(repdatessub.length());
     }
     
     obs(k, _ ) = countVecRep;
     full(k, _ ) = countVec;
   }
   
   List L = List::create(obs, full);
   
   return L;
 }
