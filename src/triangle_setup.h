#ifndef RAYVERTEX_TRIANGLE_SETUP_H
#define RAYVERTEX_TRIANGLE_SETUP_H

#include <array>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>
#include "defines.h"
#include "raster_utils.h"

inline Float DifferenceOfProducts(Float a, Float b, Float c, Float d) {
  Float cd = c * d;
  Float err = std::fma(-c, d, cd);
  Float dop = std::fma(a, b, -cd);
  return dop + err;
}
inline Float edgeFunction(const vec3& a, const vec3& b, const vec3& c) {
  return DifferenceOfProducts(c.x-a.x, b.y-a.y, c.y-a.y, b.x-a.x);
}

// No shader-varying payload here. Original face identity addresses the separate
// attribute arrays, including independent normal/UV indices and material seams.
struct TriangleSetup {
  std::array<vec3,3> vertices;
  vec3 inverse_w, step_x, step_y;
  Float inverse_area;
  int xmin, xmax, ymin, ymax;
  int face, material, culling;
};

class TriangleBins {
  int width_, height_, block_, columns_, rows_;
  std::vector<std::size_t> offsets_;
  std::vector<std::uint32_t> references_;
public:
  std::vector<TriangleSetup> triangles;
  std::size_t attempted = 0, culled = 0;
  TriangleBins(int width, int height, int block)
    : width_(width), height_(height), block_(block),
      columns_(1+(width-1)/block), rows_(1+(height-1)/block),
      offsets_(checked_samples(columns_,rows_)+1, 0) {}
  std::size_t size() const { return offsets_.size()-1; }
  bool active(std::size_t tile) const { return offsets_[tile]!=offsets_[tile+1]; }
  std::size_t begin(std::size_t tile) const { return offsets_[tile]; }
  std::size_t end(std::size_t tile) const { return offsets_[tile+1]; }
  const TriangleSetup& at(std::size_t reference) const { return triangles[references_[reference]]; }
  vec2 minimum(std::size_t tile) const { return vec2((tile/rows_)*block_,(tile%rows_)*block_); }
  vec2 maximum(std::size_t tile) const {
    auto lo=minimum(tile);
    return vec2(std::min(int(lo.x)+block_,width_),std::min(int(lo.y)+block_,height_));
  }
  void add(const std::array<vec4,3>& clip, int face, int material, int culling, bool depth) {
    ++attempted;
    TriangleSetup t;
    t.inverse_w=vec3(1.0f/clip[0].w,1.0f/clip[1].w,1.0f/clip[2].w);
    for(int k=0;k<3;++k) t.vertices[k]=clip[k]*t.inverse_w[k];
    const auto& a=t.vertices[0]; const auto& b=t.vertices[1]; const auto& c=t.vertices[2];
    const bool front = culling==1 ? cross(b-a,c-b).z>0 : culling==2 ? cross(b-a,c-b).z<0 : true;
    if(!depth && !front) { ++culled; return; }
    Float area=edgeFunction(c,b,a);
    if(area==0.0f) { ++culled; return; }
    t.inverse_area=1.0f/area;
    t.step_y=vec3(-(b.x-c.x),-(c.x-a.x),-(a.x-b.x));
    t.step_x=vec3(b.y-c.y,c.y-a.y,a.y-b.y);
    t.xmin=std::min(std::max(int(floor(fmin(a.x,fmin(b.x,c.x)))),0),width_);
    t.xmax=std::max(std::min(int(ceil(fmax(a.x,fmax(b.x,c.x)))),width_),0);
    t.ymin=std::min(std::max(int(floor(fmin(a.y,fmin(b.y,c.y)))),0),height_);
    t.ymax=std::max(std::min(int(ceil(fmax(a.y,fmax(b.y,c.y)))),height_),0);
    if(t.xmin>=t.xmax || t.ymin>=t.ymax) { ++culled; return; }
    t.face=face; t.material=material; t.culling=culling;
    if(triangles.size()>=std::numeric_limits<std::uint32_t>::max())
      throw std::overflow_error("Too many raster primitives");
    triangles.push_back(t);
  }
  template<class Visit> void tiles(const TriangleSetup& t, Visit visit) const {
    for(int x=t.xmin/block_;x<1+(t.xmax-1)/block_;++x)
      for(int y=t.ymin/block_;y<1+(t.ymax-1)/block_;++y)
        visit(y+static_cast<std::size_t>(rows_)*x);
  }
  void build() {
    std::fill(offsets_.begin(),offsets_.end(),0);
    for(const auto& triangle:triangles) tiles(triangle,[&](std::size_t tile) {
      if(offsets_[tile+1]==std::numeric_limits<std::size_t>::max())
        throw std::overflow_error("Too many raster references");
      ++offsets_[tile+1];
    });
    for(std::size_t i=1;i<offsets_.size();++i) {
      if(offsets_[i]>std::numeric_limits<std::size_t>::max()-offsets_[i-1])
        throw std::overflow_error("Too many raster references");
      offsets_[i]+=offsets_[i-1];
    }
    references_.resize(offsets_.back());
    auto cursor=offsets_;
    // Deterministic count/prefix/fill. No workers-times-tiles histogram; each
    // tile retains original model/face order, including equal-depth winners.
    for(std::size_t i=0;i<triangles.size();++i)
      tiles(triangles[i],[&](std::size_t tile) { references_[cursor[tile]++]=i; });
  }
  std::size_t references() const { return references_.size(); }
  std::size_t capacity_bytes() const {
    return triangles.capacity()*sizeof(TriangleSetup)+offsets_.capacity()*sizeof(std::size_t)+
      references_.capacity()*sizeof(std::uint32_t);
  }
};
#endif
