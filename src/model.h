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
              float *emissive_texture,
              vec3 ambient, float exponent, 
              float specular_intensity, float diffuse_intensity, float emission_intensity,
              int nx_t, int ny_t, int nn_t,
              int nx_nt, int ny_nt, int nn_nt,
              int nx_st, int ny_st, int nn_st,
              int nx_et, int ny_et, int nn_et,
              bool has_normals_, bool has_texcoords,
              bool has_texture, bool has_normal_texture, bool has_specular_texture,
              bool has_emissive_texture,
              vec3 model_color, bool tbn) :
      verts(verts), inds(inds), texcoords(texcoords), normals(normals), texture(texture), 
      normal_texture(normal_texture),
      specular_texture(specular_texture), emissive_texture(emissive_texture),
      ambient(ambient), exponent(exponent), 
      specular_intensity(specular_intensity), diffuse_intensity(diffuse_intensity),
      emission_intensity(emission_intensity),
      nx_t(nx_t), ny_t(ny_t), nn_t(nn_t),
      nx_nt(nx_nt), ny_nt(ny_nt), nn_nt(nn_nt),
      nx_st(nx_st), ny_st(ny_st), nn_st(nn_st),
      nx_et(nx_et), ny_et(ny_et), nn_et(nn_et),
      
      has_texture(has_texture), 
      has_normal_texture(has_normal_texture), 
      has_specular_texture(has_specular_texture), has_emissive_texture(has_emissive_texture),
      tbn(tbn) {
      color = model_color;
      has_normals = normals.nrow() == verts.nrow() && has_normals_;
      has_texcoords = has_texcoords;
      specular_color = vec3(1.0f,1.0f,1.0f);
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
    //Check if memory leak, possibly due to indices
    vec3 tex(int iface, int nthvert) {
      return(vec3(texcoords(inds(iface,nthvert) - 1, 0),
                  texcoords(inds(iface,nthvert) - 1, 1),
                  0.0f));
    }
    float specular(vec3 uv) {
      return(has_specular_texture ? trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st).x : 1.0f);
    }
    vec3 emissive(vec3 uv) {
      return(has_emissive_texture ? emission_intensity*trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec3(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return (trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*2.0f - 1.0f);
    }
    vec3 diffuse(vec3 uv) {
      return(has_texture ? trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t) * diffuse_intensity : color);
    }
    vec3 get_ambient() {
      return(ambient);
    }
    float get_exponent() {
      return(exponent);
    }
    vec3 get_specular_color() {
      return(specular_color);
    }
    
    Rcpp::NumericMatrix verts;
    Rcpp::IntegerMatrix inds;
    Rcpp::NumericMatrix texcoords;
    Rcpp::NumericMatrix normals;
    float* texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;
    vec3 ambient;
    float exponent;
    float specular_intensity, diffuse_intensity, emission_intensity;
    
    int nx_t, ny_t, nn_t;
    int nx_nt, ny_nt, nn_nt;
    int nx_st, ny_st, nn_st;
    int nx_et, ny_et, nn_et;
    
    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    bool has_normals, has_texcoords;
    vec3 color, specular_color;
    bool tbn;
};




#endif
