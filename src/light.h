#ifndef LIGHTH
#define LIGHTH

#include "glm.hpp"
#include "defines.h"
#include "gtc/matrix_transform.hpp"

class Light {
public:
  Light(vec3 position, vec3 color, 
        Float constant, Float linear, Float quadratic,
        Float intensity) : 
        position(position), color(color*intensity), constant(constant),
        linear(linear), quadratic(quadratic) {}
  
  vec3 position;
  vec3 color;
  Float constant;
  Float linear;
  Float quadratic;  
  Float intensity;
  
  vec3 CalcPointLightAtten(vec3 fragPos);
  vec3 CalcLightDir(vec3 fragPos);
  
};

class DirectionalLight {
public:
  DirectionalLight(vec3 direction, vec3 color,  vec3 scene_center, vec3 light_up, Float scene_diag,
                   Float near_plane, Float far_plane, Mat vp_shadow, Mat Model, Mat shadow_inv,
                   Float intensity_) : 
    direction(direction), color(color)  {
    lightProjection = glm::ortho(-scene_diag/2, scene_diag/2, -scene_diag/2, scene_diag/2, 
                                 0.1, scene_diag);
    lightView = glm::lookAt(scene_center + direction * scene_diag,
                            scene_center,
                            light_up);
    M = vp_shadow * lightProjection * lightView * Model;
    uniform_Mshadow_ = M * shadow_inv;
    intensity = intensity_;
  }
  
  vec3 direction;
  vec3 color;
  Float intensity;
  
  Mat lightProjection;
  Mat lightView;
  Mat M;
  Mat uniform_Mshadow_;
};


#endif
