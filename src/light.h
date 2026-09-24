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
  
  struct Sample { vec3 direction, attenuation; };
  Sample sample(vec3 fragPos) const {
    const vec3 displacement=position-fragPos;
    const Float distance=glm::length(displacement);
    // Keep the legacy exceptional normalization path at zero distance; changing
    // its undefined direction is a separate numerical correction.
    const vec3 direction=distance==0 ? glm::normalize(displacement) :
      displacement*(1.0/distance);
    const Float attenuation=1.0/(constant+linear*distance+quadratic*(distance*distance));
    return {direction,attenuation*color};
  }
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
                                 0.1, 2.0*scene_diag);
    // The eye is one diagonal from the center; contain the far half too.
    // Preserve the existing bias in world-distance units under this remapping.
    shadow_bias_scale=(scene_diag-0.1)/(2.0*scene_diag-0.1);
    lightView = glm::lookAt(scene_center + direction * scene_diag,
                            scene_center,
                            light_up);
    M = vp_shadow * lightProjection * lightView * Model;
    uniform_Mshadow_ = M * shadow_inv;
    intensity = intensity_;
  }
  
  vec3 direction;
  vec3 view_direction; // View * Model direction, prepared once without renormalizing.
  vec3 color;
  Float intensity;
  
  Float shadow_bias_scale;
  Mat lightProjection;
  Mat lightView;
  Mat M;
  Mat uniform_Mshadow_;
};


#endif
