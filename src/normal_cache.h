#ifndef RAYVERTEX_NORMAL_CACHE_H
#define RAYVERTEX_NORMAL_CACHE_H

#include "defines.h"
#include <cstring>
#include <vector>

// Main-thread, frame-local cache. Matrix identity includes its exact arithmetic
// result and the shader's pre-transform normalization convention.
struct NormalTransformCache {
  Mat matrix;
  bool normalize_input;
  std::vector<vec3> values;
  std::vector<unsigned char> valid;
  std::size_t hits=0, misses=0;
  NormalTransformCache(const Mat& transform,bool normalized,std::size_t count) :
    matrix(transform),normalize_input(normalized),values(count),valid(count,0) {}
  bool matches(const Mat& transform,bool normalized) const {
    if(normalized!=normalize_input) return false;
    for(int c=0;c<4;++c) for(int r=0;r<4;++r)
      if(std::memcmp(&matrix[c][r],&transform[c][r],sizeof(Float))!=0) return false;
    return true;
  }
  std::size_t payload_bytes() const {
    return values.size()*sizeof(vec3)+valid.size();
  }
};
#endif
