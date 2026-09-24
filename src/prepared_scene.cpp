#include "prepared_scene.h"

// [[Rcpp::export]]
SEXP prepare_scene_rcpp(Rcpp::List scene) {
  Rcpp::XPtr<PreparedScene> handle(new PreparedScene(scene),true,
                                 Rf_install("rayvertex_prepared_scene_v1"));
  return handle;
}

// [[Rcpp::export]]
Rcpp::List prepared_scene_snapshot(SEXP handle) {
  return prepared_scene_owner(handle).snapshot;
}

// [[Rcpp::export]]
Rcpp::List prepared_scene_info(SEXP handle) {
  auto& owner=prepared_scene_owner(handle);
  Rcpp::NumericMatrix vertices=owner.snapshot["vertices"];
  Rcpp::List shapes=owner.snapshot["shapes"];
  std::size_t faces=0;
  for(SEXP value:shapes) {
    Rcpp::List shape(value);
    Rcpp::IntegerMatrix indices=shape["indices"];
    faces+=indices.nrow();
  }
  return Rcpp::List::create(Rcpp::Named("vertices")=vertices.nrow(),
    Rcpp::Named("triangles")=double(faces), Rcpp::Named("texture_decodes")=double(owner.textures.decodes),
    Rcpp::Named("texture_payload_bytes")=double(owner.textures.payload_bytes));
}

// [[Rcpp::export]]
Rcpp::List prepared_scene_lifetime() {
  return Rcpp::List::create(Rcpp::Named("handles")=double(RayvertexPreparedLifetime::handles),
    Rcpp::Named("texture_bytes")=double(RayvertexPreparedLifetime::texture_bytes));
}
