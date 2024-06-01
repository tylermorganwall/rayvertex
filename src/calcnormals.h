#ifndef CALCNORMALSH
#define CALCNORMALSH

#include "Rcpp.h"

Rcpp::NumericMatrix CalculateNormals(Rcpp::List raymesh, int shape_id);

#endif