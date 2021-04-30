#ifndef LINEH
#define LINEH

#include "Rcpp.h"
#include "glm.hpp"
#include "rayimage.h"
#include "alphainfo.h"
#include "defines.h"

void aa_line(std::vector<vec3>& line_mat,
             Rcpp::NumericMatrix &zbuffer,
             std::vector<std::map<Float, alpha_info> >& alpha_depths,
             vec3 color, Float alpha_line, Float line_offset);

void noaa_line(std::vector<vec3>& line_mat,
               Rcpp::NumericMatrix& zbuffer,
               std::vector<std::map<Float, alpha_info> >& alpha_depths,
               vec3& color, Float alpha_line, Float line_offset);

#endif
