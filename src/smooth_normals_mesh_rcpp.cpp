#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List smooth_normals_mesh_rcpp(List mesh_input, bool override_existing = false) {
  List mesh = Rcpp::clone(mesh_input);
  List shapes = Rcpp::as<List>(mesh["shapes"]);
  List vertices = Rcpp::as<List>(mesh["vertices"]);
  List normals = Rcpp::as<List>(mesh["normals"]);
  
  int nShapes = shapes.size();
  
  for (int s = 0; s < nShapes; s++) {
    NumericMatrix curr_vertices = Rcpp::as<NumericMatrix>(vertices[s]);
    List curr_shape = Rcpp::as<List>(shapes[s]);
    NumericMatrix curr_normals = Rcpp::as<NumericMatrix>(normals[s]);
    IntegerMatrix indices = Rcpp::as<IntegerMatrix>(curr_shape["indices"]);

    int nVertices = curr_vertices.nrow();
    int nTriangles = indices.nrow();
    
    NumericMatrix new_normals(nVertices, 3);
    NumericMatrix face_normals(nTriangles, 3);
    
    //Two pass algorithm: First calculate face normals, then sum to get vertex normals
    for (int i = 0; i < nTriangles; i++) {
      NumericVector face_normal(3);
      NumericVector vec1 = curr_vertices(indices(i, 1), _) - curr_vertices(indices(i, 0), _);
      NumericVector vec2 = curr_vertices(indices(i, 2), _) - curr_vertices(indices(i, 0), _);
      
      // Compute the cross product of the vectors
      face_normal[0] += vec1[1] * vec2[2] - vec1[2] * vec2[1];
      face_normal[1] += vec1[2] * vec2[0] - vec1[0] * vec2[2];
      face_normal[2] += vec1[0] * vec2[1] - vec1[1] * vec2[0];
      face_normals(i, _) = face_normal;
    }
    
    for (int i = 0; i < indices.nrow(); i++) {
      NumericVector normal(3);
      size_t idx1 = indices(i, 0);
      size_t idx2 = indices(i, 1);
      size_t idx3 = indices(i, 2);
      NumericVector norm = face_normals(i,_);
        
      new_normals(idx1, _) = new_normals(idx1, _) + norm;
      new_normals(idx2, _) = new_normals(idx2, _) + norm;
      new_normals(idx3, _) = new_normals(idx3, _) + norm;
    }
    
    for (int i = 0; i < new_normals.nrow(); i++) {
      double len = sqrt(sum(pow(new_normals(i, _), 2.0)));
      new_normals(i, _) = new_normals(i, _) / len;
    }
    
    // Check if all elements of the has_vertex_normals vector are FALSE
    LogicalVector has_vertex_normals = Rcpp::as<LogicalVector>(curr_shape["has_vertex_normals"]);
    bool allFalse = true;
    for (int i = 0; i < has_vertex_normals.size(); i++) {
      if (has_vertex_normals[i]) {
        allFalse = false;
        break;
      }
    }
    
    // Update the normals for the shape if override_existing is TRUE or if all normals are FALSE
    if (override_existing || allFalse) {
      normals[s] = new_normals;
      has_vertex_normals.fill(true);
      curr_shape["norm_indices"] = curr_shape["indices"];
    }
  }
  
  return mesh;
}