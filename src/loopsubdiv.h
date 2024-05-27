#ifndef LOOPSUBDIVH
#define LOOPSUBDIVH

#include "Rcpp.h"

Rcpp::List LoopSubdivide(Rcpp::List mesh,
                         int shape_i,
                         const int nLevels,
                         bool verbose);

#endif  