#ifndef MATERIALH
#define MATERIALH

#include "glm.hpp"
#include "Rcpp.h"
#include "defines.h"


struct material_info {
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
  vec3 transmittance;  
  vec3 emission;
  Float shininess;
  Float ior; 
  Float dissolve;    
  Float illum;
  Rcpp::String ambient_texname; 
  Rcpp::String diffuse_texname; 
  Rcpp::String specular_texname;  
  Rcpp::String normal_texname;
  Rcpp::String emissive_texname;
  int max_indices;
  Float emission_intensity;
  Float diffuse_intensity;
  Float specular_intensity;
  Float ambient_intensity;
  bool has_texture;
  bool has_ambient_texture;
  bool has_normal_texture;
  bool has_specular_texture;
  bool has_emissive_texture;
  int cull_type; //1 = back, 2 = front, 3 = none
  bool translucent;
  Float toon_levels;
  Float reflection_intensity;
  Float sigma;
};

#endif
