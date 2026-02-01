#include "rayimage.h"

vec4 trivalue(Float uu, Float vv,  float* data, 
              int nx, int ny, int channels) {
  // Wrap to [0,1]
  uu = uu - floor(uu);
  vv = vv - floor(vv);
  
  // Map to texel space
  Float x = uu * (Float)(nx - 1);
  Float y = (1 - vv) * (Float)(ny - 1);
  
  int x0 = (int)floor(x);
  int x1 = std::min(x0 + 1, nx - 1);
  int y0 = (int)floor(y);
  int y1 = std::min(y0 + 1, ny - 1);
  
  Float tx = x - (Float)x0;
  Float ty = y - (Float)y0;
  
  auto sample = [&](int xi, int yi) -> vec4 {
    int idx = channels*xi + channels*nx*yi;
    Float r = data[idx];
    Float g = data[idx + 1];
    Float b = data[idx + 2];
    Float a = channels == 4 ? data[idx + 3] : 1.0f;
    return vec4(r,g,b,a);
  };
  
  vec4 c00 = sample(x0,y0);
  vec4 c10 = sample(x1,y0);
  vec4 c01 = sample(x0,y1);
  vec4 c11 = sample(x1,y1);
  
  // Bilinear interpolation
  vec4 c0 = c00 * (1 - tx) + c10 * tx;
  vec4 c1 = c01 * (1 - tx) + c11 * tx;
  vec4 c = c0 * (1 - ty) + c1 * ty;
  return c;
}

vec4 trivalue(Float uu, Float vv, reflection_map_info ref) {
  int nx = ref.nx;
  int ny = ref.ny;
  int channels = ref.nn;
  
  // Wrap to [0,1]
  uu = uu - floor(uu);
  vv = vv - floor(vv);
  
  // Map to texel space
  Float x = uu * (Float)(nx - 1);
  Float y = (1 - vv) * (Float)(ny - 1);
  
  int x0 = (int)floor(x);
  int x1 = std::min(x0 + 1, nx - 1);
  int y0 = (int)floor(y);
  int y1 = std::min(y0 + 1, ny - 1);
  
  Float tx = x - (Float)x0;
  Float ty = y - (Float)y0;
  
  auto sample = [&](int xi, int yi) -> vec4 {
    int idx = channels*xi + channels*nx*yi;
    Float r = ref.reflection[idx];
    Float g = ref.reflection[idx + 1];
    Float b = ref.reflection[idx + 2];
    return vec4(r,g,b,1.0f);
  };
  
  vec4 c00 = sample(x0,y0);
  vec4 c10 = sample(x1,y0);
  vec4 c01 = sample(x0,y1);
  vec4 c11 = sample(x1,y1);
  
  vec4 c0 = c00 * (1 - tx) + c10 * tx;
  vec4 c1 = c01 * (1 - tx) + c11 * tx;
  vec4 c = c0 * (1 - ty) + c1 * ty;
  return c;
}
