#ifndef LINEH
#define LINEH

#include "Rcpp.h"
#include "glm.hpp"
#include "rayimage.h"
#include "alphainfo.h"

void aa_line(std::vector<glm::vec3>& line_mat,
             Rcpp::NumericMatrix &zbuffer,
             std::vector<std::map<float, alpha_info> >& alpha_depths,
             glm::vec3 color, float alpha_line, float line_offset);

void noaa_line(std::vector<glm::vec3>& line_mat,
               Rcpp::NumericMatrix& zbuffer,
               std::vector<std::map<float, alpha_info> >& alpha_depths,
               glm::vec3& color, float alpha_line);

#endif