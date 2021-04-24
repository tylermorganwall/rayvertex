#ifndef FILLTRIH
#define FILLTRIH

#include "glm.hpp"
#include "Rcpp.h"
#include "shaders.h"
#include "RcppThread.h"
#include "alphainfo.h"
#include "defines.h"

static void print_vec(vec3 m);
static void print_vec(vec4 m);
inline Float DifferenceOfProducts(Float a, Float b, Float c, Float d);
inline Float edgeFunction(const vec3 &a, const vec3 &b, const vec3 &c);

void fill_tri_blocks(std::vector<std::vector<int> >&  block_faces,
                     std::vector<std::vector<std::vector<vec4> >  >& ndc_verts,
                     std::vector<std::vector<std::vector<Float> > >& ndc_inv_w,
                     vec2 min_block_bound,
                     vec2 max_block_bound,
                     std::vector<IShader*> shaders,
                     Rcpp::NumericMatrix &zbuffer, 
                     rayimage& image, 
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     rayimage& uv_buffer,
                     std::vector<ModelInfo> &models,
                     bool depth, 
                     std::vector<std::map<Float, alpha_info> >& alpha_depths);

#endif