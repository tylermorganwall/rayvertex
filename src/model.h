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
    ModelInfo(Rcpp::NumericMatrix &verts, Rcpp::NumericMatrix &texcoords, Rcpp::NumericMatrix &normals,
              Rcpp::IntegerMatrix inds, Rcpp::IntegerMatrix tex_inds, Rcpp::IntegerMatrix norm_inds, 
              Rcpp::IntegerVector &materials,
              bool has_normals_, bool has_texcoords_,
              bool tbn) :
      verts(verts),  texcoords(texcoords), normals(normals), 
      inds(inds), tex_inds(tex_inds), norm_inds(norm_inds), 
      materials(materials),
      tbn(tbn) {
      has_normals = normals.nrow() == inds.nrow() && has_normals_;
      has_texcoords = has_texcoords_;
      num_indices = inds.nrow();
    }
    
    vec3 vertex(int iface, int nthvert) { 
      return(vec3(verts(inds(iface,nthvert) - 1, 0),
                  verts(inds(iface,nthvert) - 1, 1),
                  verts(inds(iface,nthvert) - 1, 2)));
    }
    vec3 normal(int iface, int nthvert) {
      return(vec3(normals(norm_inds(iface,nthvert) - 1, 0),
                  normals(norm_inds(iface,nthvert) - 1, 1),
                  normals(norm_inds(iface,nthvert) - 1, 2)));
    }
    //Check if memory leak, possibly due to indices
    vec3 tex(int iface, int nthvert) {
      return(vec3(texcoords(tex_inds(iface,nthvert) - 1, 0),
                  texcoords(tex_inds(iface,nthvert) - 1, 1),
                  0.0f));
    }
    
    Rcpp::NumericMatrix verts;
    Rcpp::NumericMatrix texcoords;
    Rcpp::NumericMatrix normals;
    
    Rcpp::IntegerMatrix inds;
    Rcpp::IntegerMatrix tex_inds;
    Rcpp::IntegerMatrix norm_inds;
    Rcpp::IntegerVector materials;
    
    bool has_normals, has_texcoords;
    bool tbn;
    int num_indices;
};




#endif
