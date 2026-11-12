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
//' @param nsamples An integer of default 100,000, representing the number of samples from the posterior distributions.
//' @returns A numerical array, each column representing posterior samples of the completeness distribution for a particular nowcast value. The last column represents the most recent number to be nowcasted. 
//' @example man/examples/LawlessNowcastProbs_example.R
//' @export
// [[Rcpp::export]]
 NumericMatrix LawlessNowcastProbs(DataFrame data, Date dateAnal, int week_start = 2, 
                      int maxdelay = 25,
                      String reference_date = "reference_date", String report_date = "report_date",int nsamples = 100000) {
   
   DateVector evdates0 = data[reference_date];
   DateVector rpdates0 = data[report_date];
   
   double alpha, beta;
   
   int tau = maxdelay + 2;
   
   Date weekstrt = dateAnal;
   
   while(weekstrt.getWeekday()!=week_start) {
     weekstrt = Date(weekstrt + -1);
   }
   
   DateVector evdates(evdates0[(evdates0 >= weekstrt + -tau*7) & (evdates0 < weekstrt) & (rpdates0 < weekstrt)]);;
   DateVector rpdates(rpdates0[(evdates0 >= weekstrt + -tau*7) & (evdates0 < weekstrt) & (rpdates0 < weekstrt)]);;
   
  // double alpha, beta;
   
   DateVector week_ref(evdates.length()), week_rep(rpdates.length());
   IntegerVector lag(evdates.length());
   
  // Date week;
   for (int k=0; k < week_ref.length(); k++) {
     Date week1 = evdates[k];
     while (week1.getWeekday()!=week_start) {
       week1 = Date(week1 + -1);
     }
     week_ref[k] = week1;
     
     Date week2 = rpdates[k];
     while (week2.getWeekday()!=week_start) {
       week2 = Date(week2 + -1);
     }
     week_rep[k] = week2;
     lag[k] = int(week_rep[k] - week_ref[k])/7;
     
     if (lag[k] > maxdelay) {
       lag[k] = maxdelay + 1;
     }
   }
   
   DateVector weeks = sort_unique(week_ref);
   int j = 0;
   
   NumericMatrix gmat(nsamples,tau), pmat(nsamples,tau);
   IntegerMatrix ntmat(weeks.length(),tau),Ntmat(weeks.length(),tau);
   NumericVector gvec(tau);
   
   for(DateVector::iterator w = weeks.begin(); w != weeks.end(); w++) {
     Date week = *w;
     
     IntegerVector Ntvec(week_ref.length()), ntvec(week_ref.length());
     
     int Ntcnt = 0, ntcnt;
     for(int i=0; i<tau; i++) {
       
       IntegerVector weeklagsub;
       
       for (int k=0; k < week_ref.length(); k++) {
         if((lag(k)==i) & (Date(week_ref(k))==week)) {
           weeklagsub.push_front(lag(k));
         }
       }
       ntcnt = weeklagsub.length();
       Ntcnt += ntcnt;
       ntvec(i) = ntcnt;
       Ntvec(i) = Ntcnt;
     }
     
     ntmat( _ ,j) = ntvec;
     Ntmat( _ ,j) = Ntvec;
     
     j++;
     
   }
   
   for(int i=0; i<tau; i++) {
     
     NumericVector gvectot((tau + -i)*nsamples),gvec(nsamples);
     
     for(int k=0; k<(tau + -i); k++) {
       alpha = ntmat(i,k) + 1;
       beta = Ntmat(i,k) - ntmat(i,k) + 1;
       for(int l=0; l<nsamples; l++) {
         gvectot(k*nsamples + l) = R::rbeta(alpha,beta);
       }
     }
     IntegerVector full(gvectot.length());
     
     for(int m=0; m<gvectot.length(); m++){
       full(m) = m;
     }
     
     IntegerVector smple = sample(full,nsamples);
     
     for(int n=0; n<nsamples; n++) {
       gvec(n) = gvectot(smple(n));
     }
     gmat( _ ,i) = gvec;
   }
   
   double gprod;
   
   for(int k=0; k<nsamples; k++) {
     gvec = gmat(k,_);
     NumericVector pvec(tau);
     for(int i=0; i<tau; i++) {
       gprod = 1;
       for(int l=i+1; l<tau; l++) {
         gprod *= (1-gvec[l]);
       }
       pvec(i) = gprod;
     }
     pmat(k,_) = rev(pvec);
   }
   return pmat;
 }