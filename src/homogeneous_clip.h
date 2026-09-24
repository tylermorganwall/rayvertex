#ifndef RAYVERTEX_HOMOGENEOUS_CLIP_H
#define RAYVERTEX_HOMOGENEOUS_CLIP_H

#include <array>
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
#include "defines.h"

struct ClipVertex {
  vec4 position;
  vec3 weights; // original primitive's independent attribute interpolation
};
struct ClippedPolygon {
  // Clipping a triangle by seven convex halfspaces adds at most seven vertices.
  std::array<ClipVertex,10> vertices;
  std::size_t size=0;
  bool changed=false;
};

// Shaders return viewport-composed homogeneous coordinates. Test equivalent
// frustum planes directly, avoiding inverse-viewport rounding of untouched
// primitives. Width/height=1 are degenerate viewports with no covered samples.
inline ClippedPolygon clip_homogeneous(const std::array<vec4,3>& input, int width, int height) {
  ClippedPolygon polygon;
  for(int i=0;i<3;++i) {
    for(int k=0;k<4;++k)
      if(!std::isfinite(input[i][k]))
        throw std::invalid_argument("Nonfinite transformed raster vertex");
    polygon.vertices[i]={input[i],vec3(i==0,i==1,i==2)};
  }
  if(width<=1 || height<=1) return polygon;
  polygon.size=3;
  auto distance=[&](const vec4& p,int plane) {
    switch(plane) {
      case 0: return p.x;
      case 1: return Float(width-1)*p.w-p.x;
      case 2: return p.y;
      case 3: return Float(height-1)*p.w-p.y;
      case 4: return p.z;
      case 5: return p.w-p.z;
      default: return p.w-std::numeric_limits<Float>::min();
    }
  };
  unsigned outside_any=0, outside_all=127;
  for(const auto& p:input) {
    unsigned outside=0;
    for(int plane=0;plane<7;++plane) {
      Float d=distance(p,plane);
      if(!std::isfinite(d)) throw std::invalid_argument("Raster clip-plane distance overflow");
      outside |= unsigned(d<0)<<plane;
    }
    outside_any |= outside;
    outside_all &= outside;
  }
  if(!outside_any) return polygon;
  if(outside_all) { polygon.size=0; polygon.changed=true; return polygon; }
  for(int plane=0;plane<7 && polygon.size;++plane) {
    ClippedPolygon output;
    output.changed=polygon.changed;
    auto append=[&](const ClipVertex& vertex) {
      if(output.size==output.vertices.size())
        throw std::overflow_error("Raster clipping capacity exceeded");
      output.vertices[output.size++]=vertex;
    };
    for(std::size_t i=0;i<polygon.size;++i) {
      const auto& a=polygon.vertices[i];
      const auto& b=polygon.vertices[(i+1)%polygon.size];
      Float da=distance(a.position,plane), db=distance(b.position,plane);
      if(!std::isfinite(da) || !std::isfinite(db))
        throw std::invalid_argument("Raster clip-plane distance overflow");
      const bool inside_a=da>=0, inside_b=db>=0;
      if(inside_a) append(a);
      else output.changed=true;
      if(inside_a!=inside_b && da!=0 && db!=0) {
        // Opposite signs: scaled ratio avoids overflow in da-db.
        Float scale=std::max(std::abs(da),std::abs(db));
        Float aa=std::abs(da)/scale, bb=std::abs(db)/scale;
        Float wa=bb/(aa+bb), wb=aa/(aa+bb);
        ClipVertex crossing;
        crossing.position=wa*a.position+wb*b.position;
        crossing.weights=wa*a.weights+wb*b.weights;
        append(crossing);
        output.changed=true;
      }
    }
    polygon=output;
  }
  return polygon;
}
#endif
