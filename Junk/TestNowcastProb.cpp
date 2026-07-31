#include <Rcpp.h>
using namespace Rcpp;

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
