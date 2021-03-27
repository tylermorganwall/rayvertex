#ifndef MATERIALH
#define MATERIALH

#include "glm.hpp"
#include "Rcpp.h"


struct material_info {
  glm::vec3 ambient;
  glm::vec3 diffuse;
  glm::vec3 specular;
  glm::vec3 transmittance;  
  glm::vec3 emission;
  float shininess;
  float ior; 
  float dissolve;    
  float illum;
  Rcpp::String ambient_texname; 
  Rcpp::String diffuse_texname; 
  Rcpp::String specular_texname;  
  Rcpp::String normal_texname;
  Rcpp::String emissive_texname;
  int max_indices;
  float emission_intensity;
  float diffuse_intensity;
  float specular_intensity;
  bool has_normals;
  bool has_tex;
  bool has_texture;
  bool has_normal_texture;
  bool has_specular_texture;
  bool has_emissive_texture;
};

#endif