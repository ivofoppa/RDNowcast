#ifndef NOWCAST_H
#define NOWCAST_H

#include <Rcpp.h>
Rcpp::IntegerMatrix Nowcast(Rcpp::DataFrame data, Rcpp::Date dateAnal,int offset, Rcpp::DateVector NCdates, 
                            int NCsize, Rcpp::String reference_date, Rcpp::String report_date, 
                            Rcpp::String unit,int nsamples);

#endif

