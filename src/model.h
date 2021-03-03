#ifndef MODELH
#define MODELH

#include "glm.hpp"
#include "Rcpp.h"
#include "rayimage.h"

typedef glm::vec4 vec4;
typedef glm::vec3 vec3;
typedef glm::vec2 vec2;
typedef glm::mat4x4 Mat;

class ModelInfo {
  public:
    ModelInfo(Rcpp::NumericMatrix& verts, Rcpp::IntegerMatrix& inds, 
              Rcpp::NumericMatrix& texcoords, Rcpp::NumericMatrix& normals,
              float *texture, float *normal_texture, 
              float *specular_texture,
              vec3 ambient, float exponent, 
              float specular_intensity, float diffuse_intensity,
              int nx_t, int ny_t, int nn_t,
              int nx_nt, int ny_nt, int nn_nt,
              bool has_texture, bool has_normal_texture, bool has_specular_texture,
              vec3 model_color, bool tbn) :
      verts(verts), inds(inds), texcoords(texcoords), normals(normals), texture(texture), 
      normal_texture(normal_texture),
      specular_texture(specular_texture),
      ambient(ambient), exponent(exponent), 
      specular_intensity(specular_intensity), diffuse_intensity(diffuse_intensity),
      nx_t(nx_t), ny_t(ny_t), nn_t(nn_t),
      nx_nt(nx_nt), ny_nt(ny_nt), nn_nt(nn_nt),
      has_texture(has_texture), 
      has_normal_texture(has_normal_texture), 
      has_specular_texture(has_specular_texture), tbn(tbn) {
      color = model_color;
      has_normals = normals.nrow() == verts.nrow();
    }
    
    vec3 vertex(int iface, int nthvert) {
      return(vec3(verts(inds(iface,nthvert) - 1, 0),
                  verts(inds(iface,nthvert) - 1, 1),
                  verts(inds(iface,nthvert) - 1, 2)));
    }
    vec3 normal(int iface, int nthvert) {
      return(vec3(normals(inds(iface,nthvert) - 1, 0),
                  normals(inds(iface,nthvert) - 1, 1),
                  normals(inds(iface,nthvert) - 1, 2)));
    }
    vec3 tex(int iface, int nthvert) {
      return(vec3(texcoords(inds(iface,nthvert) - 1, 0),
                  texcoords(inds(iface,nthvert) - 1, 1),
                  0.0f));
    }
    float specular(vec3 uv) {
      return(has_specular_texture ? trivalue(uv.x,uv.y,specular_texture, nx_t, ny_t, nn_t).x : 1.0f);
    }
    vec3 normal_uv(vec3 uv) {
      vec3 n = trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt);
      return(tbn ? n : n * 2.0f - 1.0f);
    }
    vec3 diffuse(vec3 uv) {
      return(has_texture ? trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t) : color);
    }
    vec3 get_ambient() {
      return(ambient);
    }
    float get_exponent() {
      return(exponent);
    }
    
    Rcpp::NumericMatrix verts;
    Rcpp::IntegerMatrix inds;
    Rcpp::NumericMatrix texcoords;
    Rcpp::NumericMatrix normals;
    float* texture;
    float* normal_texture;
    float* specular_texture;
    vec3 ambient;
    float exponent;
    float specular_intensity, diffuse_intensity;
    
    int nx_t, ny_t, nn_t;
    int nx_nt, ny_nt, nn_nt;
    bool has_texture, has_normal_texture, has_specular_texture;
    bool has_normals;
    vec3 color;
    bool tbn;
    
};




#endif
