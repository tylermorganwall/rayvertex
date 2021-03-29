#ifndef LIGHTH
#define LIGHTH

#include "glm.hpp"

class Light {
public:
  Light(glm::vec3 position, glm::vec3 color, 
        float constant, float linear, float quadratic) : 
        position(position), color(color), constant(constant),
        linear(linear), quadratic(quadratic) {}
  
  glm::vec3 position;
  glm::vec3 color;
  float constant;
  float linear;
  float quadratic;  
  
  glm::vec3 CalcPointLightAtten(glm::vec3 fragPos);
  glm::vec3 CalcLightDir(glm::vec3 fragPos);
  
};


#endif