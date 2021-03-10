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
    ModelInfo(Rcpp::NumericMatrix verts, Rcpp::IntegerMatrix inds, 
              Rcpp::NumericMatrix texcoords, Rcpp::NumericMatrix normals,
              Rcpp::IntegerVector materials,
              bool has_normals_, bool has_texcoords_,
              bool tbn) :
      verts(verts), inds(inds), texcoords(texcoords), normals(normals), materials(materials),
      tbn(tbn) {
      has_normals = normals.nrow() == verts.nrow() && has_normals_;
      has_texcoords = has_texcoords_;
      num_indices = inds.nrow();
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
    
    Rcpp::NumericMatrix verts;
    Rcpp::IntegerMatrix inds;
    Rcpp::NumericMatrix texcoords;
    Rcpp::NumericMatrix normals;
    Rcpp::IntegerVector materials;
    
    bool has_normals, has_texcoords;
    bool tbn;
    int num_indices;
};




#endif
