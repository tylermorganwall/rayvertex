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
#include "homogeneous_clip.h"

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
  int clip_weights = -1;
};

class TriangleBins {
  static int block_count(int size,int block) {
    if(size<=0 || block<=0) throw std::invalid_argument("Invalid raster block dimensions");
    return 1+(size-1)/block;
  }
  int width_, height_, block_, columns_, rows_;
  std::vector<std::size_t> offsets_;
  std::vector<std::uint32_t> references_;
public:
  std::vector<TriangleSetup> triangles;
  std::vector<std::array<vec3,3>> clip_weights;
  std::size_t input_primitives = 0;
  std::size_t attempted = 0, culled = 0;
  TriangleBins(int width, int height, int block)
    : width_(width), height_(height), block_(block),
      columns_(block_count(width,block)), rows_(block_count(height,block)),
      offsets_(checked_samples(columns_,rows_)+1, 0) {}
  std::size_t size() const { return offsets_.size()-1; }
  bool active(std::size_t tile) const { return offsets_[tile]!=offsets_[tile+1]; }
  std::size_t begin(std::size_t tile) const { return offsets_[tile]; }
  std::size_t end(std::size_t tile) const { return offsets_[tile+1]; }
  const TriangleSetup& at(std::size_t reference) const { return triangles[references_[reference]]; }
  vec2 minimum(std::size_t tile) const { return vec2((tile/rows_)*block_,(tile%rows_)*block_); }
  vec2 maximum(std::size_t tile) const {
    auto lo=minimum(tile);
    return vec2(raster_block_end(int(lo.x),block_,width_),raster_block_end(int(lo.y),block_,height_));
  }
  void add_clipped(const std::array<vec4,3>& clip, int face, int material, int culling, bool depth) {
    ++input_primitives;
    const auto polygon=clip_homogeneous(clip,width_,height_);
    if(!polygon.changed && polygon.size==3) { add(clip,face,material,culling,depth); return; }
    for(std::size_t i=1;i+1<polygon.size;++i) {
      std::size_t previous=triangles.size();
      add({polygon.vertices[0].position,polygon.vertices[i].position,polygon.vertices[i+1].position},
          face,material,culling,depth);
      if(triangles.size()!=previous) {
        if(clip_weights.size()>=std::size_t(std::numeric_limits<int>::max()))
          throw std::overflow_error("Too many clipped raster primitives");
        triangles.back().clip_weights=clip_weights.size();
        clip_weights.push_back({polygon.vertices[0].weights,polygon.vertices[i].weights,polygon.vertices[i+1].weights});
      }
    }
  }
  void add(const std::array<vec4,3>& clip, int face, int material, int culling, bool depth) {
    ++attempted;
    TriangleSetup t;
    t.inverse_w=vec3(1.0f/clip[0].w,1.0f/clip[1].w,1.0f/clip[2].w);
    for(int k=0;k<3;++k) {
      if(!(clip[k].w>0) || !std::isfinite(t.inverse_w[k])) { ++culled; return; }
      t.vertices[k]=clip[k]*t.inverse_w[k];
      for(int j=0;j<3;++j)
        if(!std::isfinite(t.vertices[k][j]))
          throw std::invalid_argument("Nonfinite projected raster vertex");
    }
    const auto& a=t.vertices[0]; const auto& b=t.vertices[1]; const auto& c=t.vertices[2];
    const bool front = culling==1 ? cross(b-a,c-b).z>0 : culling==2 ? cross(b-a,c-b).z<0 : true;
    if(!depth && !front) { ++culled; return; }
    Float area=edgeFunction(c,b,a);
    if(area==0.0f || !std::isfinite(area)) { ++culled; return; }
    t.inverse_area=1.0f/area;
    if(!std::isfinite(t.inverse_area)) { ++culled; return; }
    t.step_y=vec3(-(b.x-c.x),-(c.x-a.x),-(a.x-b.x));
    t.step_x=vec3(b.y-c.y,c.y-a.y,a.y-b.y);
    // Clamp before integer conversion, including excessively large projections.
    auto bounded=[](Float value,int limit) { return int(std::max(0.0,std::min(value,Float(limit)))); };
    t.xmin=bounded(floor(fmin(a.x,fmin(b.x,c.x))),width_);
    t.xmax=bounded(ceil(fmax(a.x,fmax(b.x,c.x))),width_);
    t.ymin=bounded(floor(fmin(a.y,fmin(b.y,c.y))),height_);
    t.ymax=bounded(ceil(fmax(a.y,fmax(b.y,c.y))),height_);
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
      references_.capacity()*sizeof(std::uint32_t)+clip_weights.capacity()*sizeof(std::array<vec3,3>);
  }
};
#endif
