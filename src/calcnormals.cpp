#include "calcnormals.h"
#include "glm.hpp"
#include "defines.h"
#include "Rcpp.h"

using namespace Rcpp;

inline bool any_is_nan(const vec3& c) {
  return(std::isnan(c[0]) || std::isnan(c[1]) || std::isnan(c[2]));
}

// Calculate normal for a given face using three vertices
vec3 CalculateFaceNormal(const vec3& p0, const vec3& p1, const vec3& p2) {
  vec3 v0 = p1 - p0;
  vec3 v1 = p2 - p0;
  vec3 cross_product = cross(v0, v1);
  return(glm::normalize(cross_product));
}

// [[Rcpp::export]]
NumericMatrix CalculateNormals(List raymesh, int shape_id) {
  List vertex_list = as<List>(raymesh["vertices"]);
  List shape_list = as<List>(raymesh["shapes"]);
  
  NumericMatrix vertices = as<NumericMatrix>(vertex_list(shape_id));
  List shape = as<List>(shape_list(shape_id));
  IntegerMatrix idx = as<IntegerMatrix>(shape["indices"]);
  int vertexCount = vertices.nrow();
  int triangleCount = idx.nrow();
  
  // Initialize vertex normals to zero
  std::vector<vec3> vertexNormals(vertexCount);
  for (int i = 0; i < vertexCount; i++) {
    vertexNormals[i] = vec3(0.f, 0.f, 0.f);
  }
  std::vector<vec3> vertexArray(vertexCount);
  for (int i = 0; i < vertexCount; i++) {
    vertexArray[i] = vec3(vertices(i,0), vertices(i,1),vertices(i,2));
  }
  
  // std::vector<int> triangleArray = trianglemesh->vertexIndices;
  IntegerMatrix triangleArray = transpose(idx);
  // point3f* vertexArray = trianglemesh->p.get();
  
  // Calculate normals for each face and accumulate at each vertex
  for (int k = 0; k < triangleCount * 3; k += 3) {
    int i0 = triangleArray[k+0];
    int i1 = triangleArray[k+1];
    int i2 = triangleArray[k+2];
    // ASSERT(i0 < vertexCount);
    // ASSERT(i1 < vertexCount);
    // ASSERT(i2 < vertexCount);
    
    vec3 p0 = vertexArray[i0];
    vec3 p1 = vertexArray[i1];
    vec3 p2 = vertexArray[i2];
    
    vec3 faceNormal = CalculateFaceNormal(p0, p1, p2);
    
    if(any_is_nan(faceNormal)) {
      continue;
    }
    vertexNormals[i0] += faceNormal;
    vertexNormals[i1] += faceNormal;
    vertexNormals[i2] += faceNormal;
  }
  
  // Normalize the vertex normals
  for (int i = 0; i < vertexCount; i++) {
    vertexNormals[i] = glm::normalize(vertexNormals[i]);
  }
  NumericMatrix newNormals(vertexCount, 3);
  for (int i = 0; i < vertexCount; i++) {
    newNormals(i,0) = vertexNormals[i][0];
    newNormals(i,1) = vertexNormals[i][1];
    newNormals(i,2) = vertexNormals[i][2];
  }
  return(newNormals);
}