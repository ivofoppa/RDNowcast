#include <Rcpp.h>
#include "funN.h"
using namespace Rcpp;
//[Rcpp::interfaces(cpp)]
//' Generates posterior probabilities for Nowcasting, based on Lawless, 1994.
 //' @description
 //' This function generates posterior completeness probabilities for Nowcasting full numbers, based on the approach of Lawless, 1994.
 //' @param data A data frame containing data, with one column representing event times and one with reporting times. The data should be complete in the sense, that reporting delays no longer play a role. 
 //' @param dateAnal A date, represented in the format "YYYY-mm-dd" that sets the date of analysis; if missing, the most recent report_date + 1 is chosen.
 //' @param week_start An integer, denoting start of the week (1: Sunday); default value is 2 (Monday).
 //' @param maxdelay An integer, expressing the maximum lags to be considered (in weeks).
 //' @param reference_date A string variable representing the name of the column with event dates.
 //' @param report_date A string variable representing the name of the column with reporting dates.
 //' @param unit A string variable, specifying the time unit to be used (week of day)
 //' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
 //' @returns A numerical array, each column representing posterior samples of the completeness distribution for a particular nowcast value. The last column represents the most recent number to be nowcasted. 
 //' @example man/examples/LawlessNowcastProbs_example.R
 //' @export
 // [[Rcpp::export]]
 IntegerMatrix Test1(DataFrame data, Date dateAnal, Date recentRef,
                     int week_start = 2, int maxdelay = 25,
                     String reference_date = "reference_date", String report_date = "report_date", String unit = "week", 
                     int nsamples = 100000) {
   
   DateVector evdates0 = data[reference_date];
   DateVector rpdates0 = data[report_date];
   
   double alpha, beta;
   
   int tau = maxdelay + 2;
   
   DateVector evdates(evdates0[(evdates0 >= recentRef + -tau) & (evdates0 <= recentRef) & (rpdates0 < dateAnal)]);
   DateVector rpdates(rpdates0[(evdates0 >= recentRef + -tau) & (evdates0 <= recentRef) & (rpdates0 < dateAnal)]);
   
   Date rpdate, evdate;
   IntegerVector lag(evdates.length()), weekdays(evdates.length());
   
   for (int k=0; k < evdates.length(); k++) {
     
     evdate = evdates(k);
     rpdate = rpdates(k);
     
     lag(k) = int(rpdate - evdate);
     weekdays(k) = evdate.getWeekday();
     
     if (lag(k) > maxdelay) {
       lag(k) = maxdelay + 1;
     }
   }
   
   IntegerVector wdays = {1,2,3,4,5,6,7};
   
   
   NumericMatrix gmat(nsamples,tau), pmat(nsamples,tau);
   IntegerMatrix ntmat(7,tau),Ntmat(7,tau);
   NumericVector gvec(tau);
   
   List wdayNtMat, wdayntMat;
   
   for(IntegerVector::iterator d = wdays.begin(); d != wdays.end(); d++) {
     int wday = *d;
     
     IntegerVector Ntvec(tau), ntvec(tau);
     DateVector evdatessub(evdates[(weekdays==wday)]);
     DateVector uniqueDates = unique(evdatessub).sort();
     IntegerMatrix ntMat(uniqueDates.length(),tau), NtMat(uniqueDates.length(),tau);
     
     NumericVector gvectot(nsamples*uniqueDates.length());
     IntegerMatrix nt(nsamples*uniqueDates.length(),tau);
     
     int j = 0;
     
     for(DateVector::iterator e = uniqueDates.begin(); e != uniqueDates.end(); e++) {
       
       DateVector evdatessubsub(evdates[evdates==Date(*e)]);
       IntegerVector lagsubsub(lag[evdates==Date(*e)]);
       
       int Ntcnt = 0;
       for(int i=0; i<tau; i++) {
         int ntcnt = 0;
         for(int k=0; k<evdatessubsub.length(); k++) {
           
           if(lagsubsub(k)==i) {
             ntcnt++;
           }
         }
         Ntcnt += ntcnt;
         ntvec(i) = ntcnt;
         Ntvec(i) = Ntcnt;
       }
       
       ntMat(j, _ ) = ntvec;
       NtMat(j, _ ) = Ntvec;
     }
     
     wdayntMat[j] = ntMat;
     wdayNtMat[j] = NtMat;
     
     j++;
   }
   
   return wdayntMat[2];
 }