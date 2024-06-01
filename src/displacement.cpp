#include "displacement.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb/stb_image_write.h"
#define TINYEXR_IMPLEMENTATION
#define TINYEXR_USE_MINIZ 0
#define TINYEXR_USE_STB_ZLIB 1
#include "tinyobj/tinyexr.h"
#ifndef STBIMAGEH
#define STBIMAGEH
#include "stb/stb_image.h"
#endif
// #include "assert.h"
// #include "trianglemesh.h"
#include <filesystem> // C++17
namespace fs = std::filesystem;
// #include "texturecache.h"
#include "Rcpp.h"
using namespace Rcpp;
#include "defines.h"
#include "assert.h"

inline bool any_is_nan(const vec3& c) {
  return(std::isnan(c[0]) || std::isnan(c[1]) || std::isnan(c[2]));
}

// [[Rcpp::export]]
NumericMatrix DisplaceMesh(Rcpp::List raymesh,
                           NumericMatrix displacement_texture,
                           NumericMatrix displacement_texture_y,
                           NumericMatrix displacement_texture_z,
                           NumericMatrix tangents,
                           LogicalVector tangent_right_handed,
                           double displacement_scale,
                           bool displacement_vector,
                           int shape_id) {
  NumericMatrix texcoords = as<NumericMatrix>(as<List>(raymesh["texcoords"])(shape_id));
  NumericMatrix normals = as<NumericMatrix>(as<List>(raymesh["normals"])(shape_id));
  NumericMatrix vertices = as<NumericMatrix>(as<List>(raymesh["vertices"])(shape_id));
  std::vector<vec3> p(vertices.nrow());
  for(int i = 0; i < vertices.nrow(); i++) {
    p[i] = vec3(vertices(i,0),vertices(i,1),vertices(i,2));
  }
  bool has_tex = texcoords.nrow() > 0;
  int nNormals = normals.nrow();
  int nTex = texcoords.nrow();
  int nVertices = vertices.nrow();

  if(!has_tex) {
    throw std::runtime_error("Texcoords required for displacement mapping: no texcoords on mesh.");
  }
  if(nNormals != nVertices  ||
     nNormals != nTex ) {
    throw std::runtime_error("Number of normals (" + std::to_string(nNormals) +
                             ") and UV coords (" +  std::to_string(nTex) +
                             ") in mesh must be exactly equal to number of vertices (" +  
                             std::to_string(nVertices) +
                             ")for displacement mapping.");
  }
  int nx = displacement_texture.nrow()-1;
  int ny = displacement_texture.ncol()-1;
  
  if(!displacement_vector) {
    for(size_t i = 0; i < nVertices; i++) {
      vec2 uv = vec2(texcoords(i,0),texcoords(i,1));
      vec3 pp = vec3(vertices(i,0),vertices(i,1),vertices(i,2));
      int ii = uv[0] * (double)nx;
      int jj = (1-uv[1]) * (double)ny;
      
      if (ii < 0) ii = 0;
      if (jj < 0) jj = 0;
      if (ii > nx) ii = nx;
      if (jj > ny) jj = ny;
      
      double disp = displacement_texture(ii, jj);

      vec3 displace_n = displacement_scale * glm::normalize(vec3(normals(i,0),
                                                                 normals(i,1),
                                                                 normals(i,2))) * disp;
      p[i] += displace_n;
    }
  } else {
    ASSERT(base_mesh->nVertices == base_mesh->nTex);
    ASSERT(base_mesh->nVertices == base_mesh->tangent_right_handed.size());

    for(size_t i = 0; i < nVertices; i++) {
      vec2 uv = vec2(texcoords(i,0),texcoords(i,1));
      vec3 pp = vec3(vertices(i,0),vertices(i,1),vertices(i,2));
      int ii = uv[0] * (double)nx;
      int jj = (1-uv[1]) * (double)ny;
      
      if (ii < 0) ii = 0;
      if (jj < 0) jj = 0;
      if (ii > nx) ii = nx;
      if (jj > ny) jj = ny;
      double disp_x = displacement_texture(ii, jj);
      double disp_y = displacement_texture_y(ii, jj);
      double disp_z = displacement_texture_z(ii, jj);
      
      vec3 disp = normalize(vec3(disp_x,disp_y,disp_z));
      vec3 tangent = normalize(vec3(tangents(i,0),tangents(i,1),tangents(i,2)));
      if(any_is_nan(tangent)) {
        continue;
      }
      vec3 n = normalize(vec3(normals(i,0),normals(i,0),normals(i,0)));
      vec3 bitangent =  tangent_right_handed[i] ? -cross(n, tangent) : cross(n, tangent);

      vec3 displace_n = displacement_scale * (tangent * disp[0] + bitangent * disp[1] + n * disp[2]);
      if(any_is_nan(displace_n)) {
        continue;
      }
      p[i] += displace_n;
    }
  }
  NumericMatrix new_vertices(vertices.nrow(),3);
    for(int i = 0; i < vertices.nrow(); i++) {
      new_vertices(i,0) = p[i][0];
      new_vertices(i,1) = p[i][1];
      new_vertices(i,2) = p[i][2];
    }
  return(new_vertices);
}