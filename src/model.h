#ifndef MODELH
#define MODELH

#include "glm.hpp"
#include "Rcpp.h"
#include "rayimage.h"
#include "defines.h"

// typedef glm::vec4 vec4;
// typedef vec3 vec3;
// typedef glm::vec2 vec2;
// typedef glm::dmat4x4 Mat;

class ModelInfo {
  public:
    ModelInfo(Rcpp::NumericMatrix &verts, Rcpp::NumericMatrix &texcoords, Rcpp::NumericMatrix &normals,
              Rcpp::IntegerMatrix inds, Rcpp::IntegerMatrix tex_inds, Rcpp::IntegerMatrix norm_inds, 
              Rcpp::LogicalVector has_vertex_tex, Rcpp::LogicalVector has_vertex_normals, 
              Rcpp::IntegerVector &materials,
              bool has_normals_, bool has_texcoords_,
              bool tbn) :
      verts(verts),  texcoords(texcoords), normals(normals), 
      inds(inds), tex_inds(tex_inds), norm_inds(norm_inds), 
      materials(materials), has_vertex_tex(has_vertex_tex), has_vertex_normals(has_vertex_normals),
      tbn(tbn) {
      num_indices = inds.nrow();
    }
    
    vec3 vertex(int iface, int nthvert) { 
      return(vec3(verts(inds(iface,nthvert), 0),
                  verts(inds(iface,nthvert), 1),
                  verts(inds(iface,nthvert), 2)));
    }
    vec3 normal(int iface, int nthvert) {
      return(vec3(normals(norm_inds(iface,nthvert), 0),
                  normals(norm_inds(iface,nthvert), 1),
                  normals(norm_inds(iface,nthvert), 2)));
    }
    vec3 tex(int iface, int nthvert) {
      return(has_vertex_tex(iface) ?  
             vec3(texcoords(tex_inds(iface,nthvert), 0),
                  texcoords(tex_inds(iface,nthvert), 1),
                  0.0f) :
              vec3(1.0));
    }
    bool model_vertex_normals(int iface) {
      return(has_vertex_normals(iface));
    }
    bool model_vertex_texcoords(int iface) {
      return(has_vertex_tex(iface));
    }
    
    Rcpp::NumericMatrix verts;
    Rcpp::NumericMatrix texcoords;
    Rcpp::NumericMatrix normals;
    
    Rcpp::IntegerMatrix inds;
    Rcpp::IntegerMatrix tex_inds;
    Rcpp::IntegerMatrix norm_inds;
    Rcpp::IntegerVector materials;
    
    Rcpp::LogicalVector has_vertex_tex;
    Rcpp::LogicalVector has_vertex_normals;
    
    bool tbn;
    int num_indices;
};




#endif
