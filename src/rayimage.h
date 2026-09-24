#ifndef RAYIMAGEH
#define RAYIMAGEH

#include "glm.hpp"
#include "Rcpp.h"
#include "defines.h"
#include "shadow_sample.h"


// typedef glm::vec4 vec4;
// typedef vec3 vec3;
// typedef glm::vec2 vec2;
// typedef glm::dmat4x4 Mat;

vec4 trivalue(Float uu, Float vv,  const float* data,
              int nx, int ny, int channels);

vec4 trivalue(Float uu, Float vv, reflection_map_info ref);

class rayimage {
  public:
    // Set on the main thread after the shadow resolve barrier.
    bool has_transparent_samples=true;
    rayimage(Rcpp::NumericMatrix &r_, Rcpp::NumericMatrix &g_, Rcpp::NumericMatrix &b_,
             int nx, int ny, Float shadow_map_intensity = 0.0f) : r(r_), g(g_), b(b_), nx(nx), ny(ny),
             shadow_map_intensity(shadow_map_intensity), enabled(r_.size()!=0) {};
    rayimage(Rcpp::NumericMatrix &r_, Rcpp::NumericMatrix &g_, Rcpp::NumericMatrix &b_, Rcpp::NumericMatrix &a_,
             int nx, int ny, Float shadow_map_intensity = 0.0f) : r(r_), g(g_), b(b_), a(a_), nx(nx), ny(ny),
             shadow_map_intensity(shadow_map_intensity), enabled(r_.size()!=0) {};
    rayimage(Rcpp::NumericMatrix &mat, 
             int nx, int ny, Float shadow_map_intensity = 0.0f) : r(mat), g(mat), b(mat), a(mat), nx(nx), ny(ny),
             shadow_map_intensity(shadow_map_intensity), enabled(mat.size()!=0) {};
    void set_color(int i, int j, vec3 col) {
      if(!enabled) return;
      r(i,j) = col.r;
      g(i,j) = col.g;
      b(i,j) = col.b;
    }
    void set_color(int i, int j, vec4 col) {
      if(!enabled) return;
      r(i,j) = col.r;
      g(i,j) = col.g;
      b(i,j) = col.b;
      a(i,j) = col.w;
    }
    vec3 get_color(int i, int j) {
      return(vec3(r(i,j),g(i,j),b(i,j)));
    }
    vec4 get_color_a(int i, int j) {
      return(vec4(r(i,j),g(i,j),b(i,j),a(i,j)));
    }
    vec3 get_color_bounded(int i, int j) {
      i = i > nx-1 ? nx-1 : i;
      j = j > ny-1 ? ny-1 : j;
      i = i < 0 ? 0 : i;
      j = j < 0 ? 0 : j;
      return(vec3(r(i,j),g(i,j),b(i,j)));
    }
    vec4 get_color_bounded_a(int i, int j) {
      i = i > nx-1 ? nx-1 : i;
      j = j > ny-1 ? ny-1 : j;
      i = i < 0 ? 0 : i;
      j = j < 0 ? 0 : j;
      return(vec4(r(i,j),g(i,j),b(i,j),a(i,j)));
    }
    Float pcf25(int i, int j, Float threshold) {
      return shadow_pcf25(r.begin(),nx,ny,i,j,threshold,shadow_map_intensity);
    }
    void set_depth(int i, int j, Float value) { r(i,j)=value; }
    int width() {
      return(nx);
    }
    int height() {
      return(ny);
    }
    Float get_shadow_intensity() {
      return(shadow_map_intensity);
    }
  private:
    Rcpp::NumericMatrix r; 
    Rcpp::NumericMatrix g; 
    Rcpp::NumericMatrix b;
    Rcpp::NumericMatrix a;
    
    int nx, ny;
    Float shadow_map_intensity;
    bool enabled;
};


#endif
