#ifndef RAYIMAGEH
#define RAYIMAGEH

#include "glm.hpp"
#include "Rcpp.h"


typedef glm::vec4 vec4;
typedef glm::vec3 vec3;
typedef glm::vec2 vec2;
typedef glm::mat4x4 Mat;

vec3 trivalue(float uu, float vv,  float* data, 
              int nx, int ny, int channels);

class rayimage {
  public:
    rayimage(Rcpp::NumericMatrix &r_, Rcpp::NumericMatrix &g_, Rcpp::NumericMatrix &b_,
             int nx, int ny) : r(r_), g(g_), b(b_), nx(nx), ny(ny) {};
    void set_color(int i, int j, vec3 col) {
      r(i,j) = col.r;
      g(i,j) = col.g;
      b(i,j) = col.b;
    }
    vec3 get_color(int i, int j) {
      return(vec3(r(i,j),g(i,j),b(i,j)));
    }
    int width() {
      return(nx);
    }
    int height() {
      return(ny);
    }
  private:
    Rcpp::NumericMatrix r; 
    Rcpp::NumericMatrix g; 
    Rcpp::NumericMatrix b;
    int nx, ny;
};


#endif