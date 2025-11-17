#ifndef RAYVERTEX_GBUFFERH
#define RAYVERTEX_GBUFFERH

#include "glm.hpp"
#include "defines.h"

struct OutlineGBufferPixel {
  vec3  normal_view;
  Float depth_view;        // >0 means geometry, <=0 means background
  std::uint32_t material_id;
  Float outline_width; 
  vec3  outline_color;
  bool  has_outline; 
};

#endif