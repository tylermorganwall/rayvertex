#ifndef MODELH
#define MODELH

#include "glm.hpp"
#include "Rcpp.h"
#include "rayimage.h"
#include "defines.h"
#include <vector>
#include <stdexcept>

struct IndexedTransforms {
  std::vector<vec4> clip;
  std::vector<vec4> viewport_clip;
  std::vector<vec3> view;
};

class ModelInfo {
  public:
    const IndexedTransforms* transforms = nullptr;
    vec4 clip_vertex(int face, int vertex_number, const Mat& mvp) {
      if (transforms && !transforms->clip.empty()) return transforms->clip[position_indices[face+std::size_t(num_indices)*vertex_number]];
      return mvp * vec4(vertex(face, vertex_number), 1.0);
    }
    vec4 viewport_vertex(int face, int vertex_number, const Mat& viewport, const Mat& mvp) {
      if (transforms && !transforms->viewport_clip.empty()) return transforms->viewport_clip[position_indices[face+std::size_t(num_indices)*vertex_number]];
      return viewport * mvp * vec4(vertex(face, vertex_number), 1.0);
    }
    vec3 view_vertex(int face, int vertex_number, const Mat& view) {
      if (transforms && !transforms->view.empty()) return transforms->view[position_indices[face+std::size_t(num_indices)*vertex_number]];
      return vec3(view * vec4(vertex(face, vertex_number), 1.0));
    }
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
      // These Rcpp owners root every array. Acquire stable views once on the R
      // thread; the frame does not mutate or resize geometry after this point.
      vertex_rows=verts.nrow(); normal_rows=normals.nrow(); texcoord_rows=texcoords.nrow();
      vertices_data=verts.begin(); normals_data=normals.begin(); texcoords_data=texcoords.begin();
      position_indices=inds.begin(); normal_indices=norm_inds.begin(); texture_indices=tex_inds.begin();
      normal_flags=has_vertex_normals.begin(); texture_flags=has_vertex_tex.begin();
      normal_flag_count=has_vertex_normals.size(); texture_flag_count=has_vertex_tex.size();
      material_data=materials.begin();
      normal_columns_valid=normals.ncol()>=3;
      const bool texture_columns_valid=texcoords.ncol()>=2;
      if(verts.ncol()<3 || inds.ncol()!=3 || tex_inds.nrow()!=num_indices ||
         norm_inds.nrow()!=num_indices || tex_inds.ncol()!=3 || norm_inds.ncol()!=3 ||
         materials.size()<num_indices)
        throw std::invalid_argument("Invalid raster geometry dimensions");
      // Validate before direct addressing, including the optional transform cache.
      for(std::size_t i=0;i<std::size_t(num_indices)*3;++i) {
        if(position_indices[i]<0 || position_indices[i]>=vertex_rows)
          throw std::out_of_range("Invalid raster position index");
        if(texture_indices[i]!=-1 && (texture_indices[i]<0 || texture_indices[i]>=texcoord_rows || !texture_columns_valid))
          throw std::out_of_range("Invalid raster texture index");
      }

    }
    
    vec3 vertex(int iface, int nthvert) {
      const int index=position_indices[iface+std::size_t(num_indices)*nthvert];
      return {vertices_data[index],vertices_data[index+vertex_rows],vertices_data[index+2*vertex_rows]};
    }
    vec3 normal(int iface, int nthvert) {
      const int index=normal_indices[iface+std::size_t(num_indices)*nthvert];
      // Missing normals are legal for faces using the existing geometric fallback.
      if(index<0 || index>=normal_rows || !normal_columns_valid)
        throw std::out_of_range("Invalid raster normal index");
      return {normals_data[index],normals_data[index+normal_rows],normals_data[index+2*normal_rows]};
    }
    vec3 tex(int iface, int nthvert) {
      const int index=texture_indices[iface+std::size_t(num_indices)*nthvert];
      if(index==-1) return vec3(1.0);
      return {texcoords_data[index],texcoords_data[index+texcoord_rows],0.0};
    }
    bool model_vertex_normals(int iface) {
      if(normal_flag_count==num_indices) return normal_flags[iface];
      const int global_face=index_offset+iface;
      if(global_face>=0 && global_face<normal_flag_count) return normal_flags[global_face];
      return has_normals;
    }
    bool model_vertex_texcoords(int iface) {
      if(texture_flag_count==num_indices) return texture_flags[iface];
      const int global_face=index_offset+iface;
      if(global_face>=0 && global_face<texture_flag_count) return texture_flags[global_face];
      return has_texcoords;
    }
    int material(int face) const { return material_data[face]; }

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
  private:
    const double *vertices_data, *normals_data, *texcoords_data;
    const int *position_indices, *normal_indices, *texture_indices;
    const int *normal_flags, *texture_flags, *material_data;
    std::size_t vertex_rows, normal_rows, texcoord_rows;
    bool normal_columns_valid;
    R_xlen_t normal_flag_count, texture_flag_count;

};




#endif
