#ifndef MODELH
#define MODELH

#include "glm.hpp"
#include "Rcpp.h"
#include "rayimage.h"
#include "defines.h"

class ModelInfo {
  public:
    ModelInfo(Rcpp::NumericMatrix &verts, Rcpp::NumericMatrix &texcoords, Rcpp::NumericMatrix &normals,
              Rcpp::IntegerMatrix inds, Rcpp::IntegerMatrix tex_inds, Rcpp::IntegerMatrix norm_inds, 
              Rcpp::LogicalVector has_vertex_tex, Rcpp::LogicalVector has_vertex_normals, 
              Rcpp::IntegerVector &materials,
              bool has_normals_, bool has_texcoords_,
              bool tbn,
              int index_offset = 0) :
      verts(verts),  texcoords(texcoords), normals(normals), 
      inds(inds), tex_inds(tex_inds), norm_inds(norm_inds), 
      materials(materials), has_vertex_tex(has_vertex_tex), has_vertex_normals(has_vertex_normals),
      tbn(tbn), has_normals(has_normals_), has_texcoords(has_texcoords_), index_offset(index_offset) {
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
      // Trust provided tex indices; fall back only if index invalid
      if(tex_inds(iface, nthvert) != -1) {
        return(vec3(texcoords(tex_inds(iface,nthvert), 0),
                    texcoords(tex_inds(iface,nthvert), 1),
                    0.0f));
      }
      return(vec3(1.0));
    }
    bool model_vertex_normals(int iface) {
      if(has_vertex_normals.size() == num_indices) {
        return(has_vertex_normals(iface));
      }
      int global_face = index_offset + iface;
      if(global_face >= 0 && global_face < has_vertex_normals.size()) {
        return(has_vertex_normals(global_face));
      }
      return(has_normals);
    }
    bool model_vertex_texcoords(int iface) {
      if(has_vertex_tex.size() == num_indices) {
        return(has_vertex_tex(iface));
      }
      int global_face = index_offset + iface;
      if(global_face >= 0 && global_face < has_vertex_tex.size()) {
        return(has_vertex_tex(global_face));
      }
      return(has_texcoords);
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
    bool has_normals;
    bool has_texcoords;
    int num_indices;
    int index_offset;
};




#endif
