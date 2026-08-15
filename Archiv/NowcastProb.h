#ifndef NOWCASTPROB_H
#define NOWCASTPROB_H

#include <Rcpp.h>
Rcpp::NumericMatrix NowcastProb(Rcpp::DataFrame df, Rcpp::DateVector NCdates, int offset, int week_start, int NCsize,  
                                Rcpp::String reference_date, Rcpp::String report_date, Rcpp::String unit, int nsamples);
#endif

