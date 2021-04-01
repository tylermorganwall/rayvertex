#ifndef RAYIMAGEH
#define RAYIMAGEH

#include "glm.hpp"
#include "Rcpp.h"


typedef glm::vec4 vec4;
typedef glm::vec3 vec3;
typedef glm::vec2 vec2;
typedef glm::mat4x4 Mat;

vec4 trivalue(float uu, float vv,  float* data, 
              int nx, int ny, int channels);

class rayimage {
  public:
    rayimage(Rcpp::NumericMatrix &r_, Rcpp::NumericMatrix &g_, Rcpp::NumericMatrix &b_,
             int nx, int ny, float shadow_map_intensity = 0.0f) : r(r_), g(g_), b(b_), nx(nx), ny(ny),
             shadow_map_intensity(shadow_map_intensity) {};
    void set_color(int i, int j, vec3 col) {
      r(i,j) = col.r;
      g(i,j) = col.g;
      b(i,j) = col.b;
    }
    vec3 get_color(int i, int j) {
      return(vec3(r(i,j),g(i,j),b(i,j)));
    }
    vec3 get_color_bounded(int i, int j) {
      i = i > nx-1 ? nx-1 : i;
      j = j > ny-1 ? ny-1 : j;
      i = i < 0 ? 0 : i;
      j = j < 0 ? 0 : j;
      return(vec3(r(i,j),g(i,j),b(i,j)));
    }
    int width() {
      return(nx);
    }
    int height() {
      return(ny);
    }
    float get_shadow_intensity() {
      return(shadow_map_intensity);
    }
  private:
    Rcpp::NumericMatrix r; 
    Rcpp::NumericMatrix g; 
    Rcpp::NumericMatrix b;
    int nx, ny;
    float shadow_map_intensity;
};


#endif