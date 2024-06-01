#ifndef DISPLACEMENTH
#define DISPLACEMENTH

#include "float.h"
#include <string>
#include "Rcpp.h"

Rcpp::NumericMatrix DisplaceMesh(Rcpp::List raymesh,
                                 Rcpp::NumericMatrix displacement_texture,
                                 Rcpp::NumericMatrix displacement_texture_y,
                                 Rcpp::NumericMatrix displacement_texture_z,
                                 Rcpp::NumericMatrix tangents,
                                 Rcpp::LogicalVector tangent_right_handed,
                                 double displacement_scale,
                                 bool displacement_vector,
                                 int shape_id);

#endif