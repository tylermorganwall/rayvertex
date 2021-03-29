#include "light.h"

glm::vec3 Light::CalcPointLightAtten(glm::vec3 fragPos) {
  float distance    = length(position - fragPos);
  float attenuation = 1.0 / (constant + linear * distance + 
                             quadratic * (distance * distance));    
  return (attenuation * color);
} 

glm::vec3 Light::CalcLightDir(glm::vec3 fragPos) {
  return (normalize(position - fragPos));
} 