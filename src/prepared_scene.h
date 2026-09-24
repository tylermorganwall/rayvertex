#ifndef RAYVERTEX_PREPARED_SCENE_H
#define RAYVERTEX_PREPARED_SCENE_H

#include "Rcpp.h"
#include "texture_cache.h"

inline int prepared_process_id() {
  Rcpp::Function getpid=Rcpp::Environment::base_env()["Sys.getpid"];
  return Rcpp::as<int>(getpid());
}
struct RayvertexPreparedLifetime {
  inline static std::size_t handles=0, texture_bytes=0;
};
inline void freeze_prepared_snapshot(SEXP value) {
  if(value==R_NilValue) return;
  if(TYPEOF(value)==VECSXP)
    for(R_xlen_t i=0;i<XLENGTH(value);++i) freeze_prepared_snapshot(VECTOR_ELT(value,i));
  // Native retention alone does not establish R copy-on-modify ownership.
  // Mark containers and atomic vectors shared before returning them to R.
  MARK_NOT_MUTABLE(value);
}

// Session-local owner. Geometry is cloned once and R-rooted by Rcpp; immutable
// material assets use shared ownership through each frame's shader lifetime.
// Camera/light matrices, shadows, effects and R callbacks remain frame inputs.
struct PreparedScene {
  Rcpp::List snapshot;
  TextureCache textures;
  int process_id;
  explicit PreparedScene(const Rcpp::List& scene)
    : snapshot(Rcpp::clone(scene)), textures(nullptr,true), process_id(prepared_process_id()) {
    stbi_ldr_to_hdr_gamma(1.0f);
    Rcpp::List materials=snapshot["materials"];
    for(SEXP value:materials) {
      Rcpp::List material(value);
      for(const char* field:{"diffuse_texname","ambient_texname","normal_texname",
                             "specular_texname","emissive_texname"}) {
        if(!material.containsElementNamed(field)) continue;
        std::string filename=Rcpp::as<std::string>(material[field]);
        if(filename.empty()) continue;
        int width,height,channels;
        if(!textures.load(filename.c_str(),&width,&height,&channels) || width<=0 || height<=0 || channels<=0)
          throw std::runtime_error("Prepared scene texture loading failed: "+filename);
      }
    }
    freeze_prepared_snapshot(snapshot);
    ++RayvertexPreparedLifetime::handles;
    RayvertexPreparedLifetime::texture_bytes+=textures.payload_bytes;
  }
  ~PreparedScene() {
    --RayvertexPreparedLifetime::handles;
    RayvertexPreparedLifetime::texture_bytes-=textures.payload_bytes;
  }
};

inline PreparedScene& prepared_scene_owner(SEXP handle) {
  if(TYPEOF(handle)!=EXTPTRSXP || R_ExternalPtrTag(handle)!=Rf_install("rayvertex_prepared_scene_v1") ||
     R_ExternalPtrAddr(handle)==nullptr)
    Rcpp::stop("Invalid or expired prepared scene. Rebuild with prepare_scene(); native snapshots cannot be restored from serialization.");
  auto& owner=*static_cast<PreparedScene*>(R_ExternalPtrAddr(handle));
  if(owner.process_id!=prepared_process_id())
    Rcpp::stop("Invalid prepared scene in a different process. Rebuild with prepare_scene().");
  return owner;
}
#endif
