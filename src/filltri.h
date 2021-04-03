#ifndef FILLTRIH
#define FILLTRIH

#include "glm.hpp"
#include "Rcpp.h"
#include "shaders.h"
#include "RcppThread.h"

struct alpha_info {
  vec4 color;
  vec3 normal;
  vec3 position;
  vec3 uv;
};


static void print_vec(vec3 m) {
  RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << "\n";
}

static void print_vec(vec4 m) {
  RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << "\n";
}

inline float DifferenceOfProducts(float a, float b, float c, float d) {
  float cd = c * d;
  float err = std::fma(-c, d, cd);
  float dop = std::fma(a, b, -cd);
  return(dop + err);
}

inline float edgeFunction(const vec3 &a, const vec3 &b, const vec3 &c) {
  return(DifferenceOfProducts((c.x - a.x),(b.y - a.y),(c.y - a.y),(b.x - a.x)));
}


void fill_tri_blocks(std::vector<std::vector<int> >&  block_faces,
                     std::vector<std::vector<std::vector<vec4> >  >& ndc_verts,
                     std::vector<std::vector<std::vector<float> > >& ndc_inv_w,
                     vec2 min_block_bound,
                     vec2 max_block_bound,
                     std::vector<IShader*> shaders,
                     Rcpp::NumericMatrix &zbuffer, 
                     rayimage& image, 
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     rayimage& uv_buffer,
                     std::vector<ModelInfo> &models,
                     bool depth, int cullin,
                     std::vector<std::map<float, alpha_info> >& alpha_depths);

#endif