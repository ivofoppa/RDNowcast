#include <Rcpp.h>
#include "funN.h"
#include "NowcastProb.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix Nowcast(DataFrame data, Date dateAnal, DateVector NCdates, int NCsize = 10,  
                          String reference_date = "reference_date", String report_date = "report_date", 
                          int week_start = 2, String unit = "week", int nsamples = 100000) {
  
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
