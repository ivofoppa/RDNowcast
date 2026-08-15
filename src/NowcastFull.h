#ifndef NOWCASTFULL_H
#define NOWCASTFULL_H

#include <Rcpp.h>
Rcpp::IntegerMatrix NowcastFull(Rcpp::DataFrame df, Rcpp::Date dateAnal, Rcpp::Date recentRef, Rcpp::DateVector NCdates, int week_start,  
                            int NCsize, Rcpp::String reference_date, Rcpp::String report_date, Rcpp::String unit, int nsamples);
#endif

