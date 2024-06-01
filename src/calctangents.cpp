#include "calctangents.h"
#include "defines.h"
#include "glm.hpp"
#include "assert.h"
using namespace Rcpp;

inline vec3 Reject(vec3 t, vec3 n) {
  return(normalize(t - dot(t,n) * n));
}

inline double DifferenceOfProducts(double a, double b, double c, double d) {
  double cd = c * d;
  double err = std::fma(-c, d, cd);
  double dop = std::fma(a, b, -cd);
  return(dop + err);
}

// [[Rcpp::export]]
List CalculateTangents(Rcpp::List raymesh, int shape_id) {
  NumericMatrix texcoords = as<NumericMatrix>(as<List>(raymesh["texcoords"])(shape_id));
  NumericMatrix normals = as<NumericMatrix>(as<List>(raymesh["normals"])(shape_id));
  NumericMatrix vertices = as<NumericMatrix>(as<List>(raymesh["vertices"])(shape_id));
  List single_shape = as<List>(raymesh["shapes"])(shape_id);
  IntegerMatrix indices = transpose(as<IntegerMatrix>(single_shape["indices"]));
  std::vector<vec3> p(vertices.nrow());
  std::vector<vec2> uv(texcoords.nrow());
  std::vector<vec3> norm(texcoords.nrow());
  
  for(int i = 0; i < vertices.nrow(); i++) {
    p[i] = vec3(vertices(i,0),vertices(i,1),vertices(i,2));
  }
  for(int i = 0; i < normals.nrow(); i++) {
    norm[i] = vec3(normals(i,0),normals(i,1),normals(i,2));
  }
  for(int i = 0; i < texcoords.nrow(); i++) {
    uv[i] = vec2(texcoords(i,0),texcoords(i,1));
  }
  int nVertices = vertices.nrow();
  
  int triangleCount = indices.ncol();
  int vertexCount = nVertices;
  
  std::vector<vec3> tangent(vertexCount); 
  std::vector<vec3> bitangent(vertexCount); 
  
  for (int i = 0; i < vertexCount; i++) {
    tangent[i] = vec3(0.f, 0.f, 0.f);
    bitangent[i] = vec3(0.f, 0.f, 0.f); 
  }
  
  IntegerVector tangent_right_handed(vertexCount);
  NumericMatrix tangentArray(vertexCount,3);
  
  // Calculate tangent and bitangent for each triangle and add to all three vertices.
  for (int k = 0; k < triangleCount * 3; k += 3) {
    int i0 = indices[k+0];
    int i1 = indices[k+1];
    int i2 = indices[k+2];
    ASSERT(i0 < triangleCount);
    ASSERT(i1 < triangleCount);
    ASSERT(i2 < triangleCount);
    ASSERT(i0 < vertexCount);
    ASSERT(i1 < vertexCount);
    ASSERT(i2 < vertexCount);
    
    vec3 p0 = p[i0];
    vec3 p1 = p[i1];
    vec3 p2 = p[i2];
    vec2 w0 = uv[i0];
    vec2 w1 = uv[i1];
    vec2 w2 = uv[i2];
    vec3 e1 = p1 - p0;
    vec3 e2 = p2 - p0;
    Float x1 = w1[0] - w0[0];
    Float x2 = w2[0] - w0[0];
    Float y1 = w1[1] - w0[1]; 
    Float y2 = w2[1] - w0[1];
    Float inv_dop = DifferenceOfProducts(x1, y2, x2, y1);
    if(inv_dop == 0) {
      continue;
    }
    Float r = 1.f / inv_dop;
    vec3 t = (e1 * y2 - e2 * y1) * r;
    vec3 b = (e2 * x1 - e1 * x2) * r;
    tangent[i0] += t;
    tangent[i1] += t;
    tangent[i2] += t;
    bitangent[i0] += b;
    bitangent[i1] += b;
    bitangent[i2] += b;
  }
  // Orthonormalize each tangent and calculate the handedness.
  for (int i = 0; i < vertexCount; i++) {
    vec3& t = tangent[i];
    vec3& b = bitangent[i];
    vec3& n = norm[i];
    vec3 tgnt = Reject(t, n);
    tangentArray(i,0) = tgnt[0];
    tangentArray(i,1) = tgnt[1];
    tangentArray(i,2) = tgnt[2];
    tangent_right_handed[i] = dot(cross(t, b), n) > 0.f;
  }
  return(List::create(_["tangent_right_handed"] = tangent_right_handed,
                      _["tangents"] = tangentArray));
}