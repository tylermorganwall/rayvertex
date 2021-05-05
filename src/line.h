#ifndef LINEH
#define LINEH

#include "Rcpp.h"
#include "glm.hpp"
#include "rayimage.h"
#include "alphainfo.h"
#include "defines.h"

void aa_line(std::vector<vec3>& line_mat_start,
             std::vector<vec3>& line_mat_end,
             std::vector<vec3>& line_color,
             Rcpp::NumericMatrix &zbuffer,
             std::vector<std::map<Float, alpha_info> >& alpha_depths,
             Float alpha_line, Float line_offset);

void noaa_line(std::vector<vec3>& line_mat_start,
               std::vector<vec3>& line_mat_end,
               std::vector<vec3>& line_color,
               Rcpp::NumericMatrix& zbuffer,
               std::vector<std::map<Float, alpha_info> >& alpha_depths,
               Float alpha_line, Float line_offset);

#endif
