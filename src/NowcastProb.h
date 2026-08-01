#ifndef NOWCASTPROB_H
#define NOWCASTPROB_H

#include <Rcpp.h>
Rcpp::NumericMatrix NowcastProb(Rcpp::DataFrame data, Rcpp::DateVector NCdates, int offset = 4, int NCsize = 10,  
                                Rcpp::String reference_date = "reference_date", Rcpp::String report_date = "report_date", 
                                Rcpp::String unit = "week",int nsamples = 100000);
#endif

