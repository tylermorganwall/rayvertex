#ifndef RAYVERTEX_TANGENT_BASIS_H
#define RAYVERTEX_TANGENT_BASIS_H

#include "defines.h"
#include <array>

inline std::array<vec3,2> scalar_tangent_basis(const vec3& e1,const vec3& e2,
    const vec3& normal,const vec2& du,const vec2& dv) {
  const glm::mat3 A{e1,e2,normal};
  const glm::mat3 AI=glm::inverse(glm::transpose(A));
  // The legacy inverse and projection are float; normalization is double.
  const vec3 i=AI*vec3(du.x,du.y,0.0),j=AI*vec3(dv.x,dv.y,0.0);
  return {glm::normalize(i),glm::normalize(j)};
}

// Specialize the inverse-transpose projection for two UV gradients. Preserve
// GLM's exact float expressions, reciprocal, signs and zero-column products.
// Keeping the latter also preserves exceptional arithmetic and signed zeros.
inline std::array<vec3,2> algebraic_tangent_basis(const vec3& e1,const vec3& e2,
    const vec3& normal,const vec2& du,const vec2& dv) {
  const glm::vec3 u(e1),v(e2),n(normal);
  const float a=v.y*n.z-v.z*n.y;
  const float b=v.x*n.z-v.z*n.x;
  const float c=v.x*n.y-v.y*n.x;
  const float reciprocal=1.0f/(+u.x*a-u.y*b+u.z*c);
  const glm::vec3 x=glm::vec3(+a,-b,+c)*reciprocal;
  const glm::vec3 y=glm::vec3(-(u.y*n.z-u.z*n.y),+(u.x*n.z-u.z*n.x),
                             -(u.x*n.y-u.y*n.x))*reciprocal;
  const glm::vec3 z=glm::vec3(+(u.y*v.z-u.z*v.y),-(u.x*v.z-u.z*v.x),
                             +(u.x*v.y-u.y*v.x))*reciprocal;
  auto project=[&](const vec2& gradient) {
    const glm::vec2 g(gradient);
    return vec3(x.x*g.x+y.x*g.y+z.x*0.0f,
                x.y*g.x+y.y*g.y+z.y*0.0f,
                x.z*g.x+y.z*g.y+z.z*0.0f);
  };
  return {glm::normalize(project(du)),glm::normalize(project(dv))};
}
#endif
