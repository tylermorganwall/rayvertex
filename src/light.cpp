#include "light.h"

vec3 Light::CalcPointLightAtten(vec3 fragPos) {
  Float distance    = length(position - fragPos);
  Float attenuation = 1.0 / (constant + linear * distance + 
                             quadratic * (distance * distance));    
  return (attenuation * color);
} 

vec3 Light::CalcLightDir(vec3 fragPos) {
  return (normalize(position - fragPos));
} 
