#ifndef LIGHTH
#define LIGHTH

#include "glm.hpp"
#include "defines.h"
#include "gtc/matrix_transform.hpp"

class Light {
public:
  Light(vec3 position, vec3 color, 
        Float constant, Float linear, Float quadratic) : 
        position(position), color(color), constant(constant),
        linear(linear), quadratic(quadratic) {}
  
  vec3 position;
  vec3 color;
  Float constant;
  Float linear;
  Float quadratic;  
  
  vec3 CalcPointLightAtten(vec3 fragPos);
  vec3 CalcLightDir(vec3 fragPos);
  
};

class DirectionalLight {
public:
  DirectionalLight(vec3 direction, vec3 color,  vec3 scene_center, vec3 light_up, Float scene_diag,
                   Float near_plane, Float far_plane, Mat vp_shadow, Mat Model, Mat shadow_inv) : 
    direction(direction), color(color) {
    lightProjection = glm::ortho(-scene_diag/2, scene_diag/2, -scene_diag/2, scene_diag/2, 
                                 near_plane, far_plane);
    lightView = glm::lookAt(scene_center + direction * scene_diag,
                            scene_center,
                            light_up);
    M = vp_shadow * lightProjection * lightView * Model;
    uniform_Mshadow_ = M * shadow_inv;
  }
  
  vec3 direction;
  vec3 color;
  Mat lightProjection;
  Mat lightView;
  Mat M;
  Mat uniform_Mshadow_;
};


#endif
