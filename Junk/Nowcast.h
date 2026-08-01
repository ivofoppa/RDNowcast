#ifndef NOWCAST_H
#define NOWCAST_H

#include <Rcpp.h>
Rcpp::IntegerMatrix Nowcast(Rcpp::DataFrame data, Rcpp::Date dateAnal, Rcpp::DateVector NCdates, 
                            int NCsize, Rcpp::String reference_date, Rcpp::String report_date, 
                            int week_start, Rcpp::String unit,int nsamples);

#endif

