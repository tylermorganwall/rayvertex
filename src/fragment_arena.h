#ifndef RAYVERTEX_FRAGMENT_ARENA_H
#define RAYVERTEX_FRAGMENT_ARENA_H

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <map>
#include <vector>
#include "alphainfo.h"
#include "raster_utils.h"

// One arena per coverage block. Raster tasks have exclusive block ownership;
// serial lines append only after their barrier. Sequence is local to a block:
// only writes to the same sample need ordering, including triangle/line ties.
class FragmentArena {
  struct Fragment {
    Float depth;
    std::size_t sequence;
    std::size_t sample;
    vec4 color;
  };
  struct Tile {
    std::vector<Fragment> fragments;
    std::vector<vec3> auxiliary;
    std::size_t max_layers = 0, touched = 0;
  };
  int width_, height_, block_, rows_;
  unsigned mask_, stride_;
  std::vector<Tile> tiles_;
  std::vector<std::map<Float, alpha_info>> reference_;
public:
  FragmentArena(int width, int height, int block = 4, unsigned mask = 7)
    : width_(width), height_(height), block_(block),
      rows_(0), mask_(mask), stride_((mask&1)+((mask>>1)&1)+((mask>>2)&1)) {
    checked_samples(width, height);
    if (block <= 0) throw std::invalid_argument("Invalid fragment block size");
    rows_ = 1 + (height-1)/block;
    tiles_.resize(checked_samples(1 + (width-1)/block, rows_));
    if (std::getenv("RAYVERTEX_REFERENCE_TRANSPARENCY"))
      reference_.resize(checked_samples(width, height));
  }
  void insert(int x, int y, Float depth, const alpha_info& value) {
    const auto sample = fragment_index(x, y, width_, height_);
    if (!reference_.empty()) { reference_[sample][depth] = value; return; }
    auto& tile = tiles_[y/block_ + static_cast<std::size_t>(rows_)*(x/block_)];
    auto& records = tile.fragments;
    // std::vector checks growth/size overflow and propagates allocation failure.
    // No reserved layer count, cap, cross-frame retention, or dropped fragments.
    records.push_back({depth, records.size(), sample, value.color});
    if(mask_&1) tile.auxiliary.push_back(value.normal);
    if(mask_&2) tile.auxiliary.push_back(value.position);
    if(mask_&4) tile.auxiliary.push_back(value.uv);
  }
  std::size_t size() const { return tiles_.size(); }
  template<class Resolve> void resolve_tile(std::size_t index, Resolve resolve) {
    auto& tile = tiles_[index];
    tile.max_layers = tile.touched = 0;
    if (!reference_.empty()) {
      const int x0 = (index/rows_)*block_, y0 = (index%rows_)*block_;
      for (int x=x0; x<std::min(x0+block_, width_); ++x)
        for (int y=y0; y<std::min(y0+block_, height_); ++y) {
          const auto& sample = reference_[fragment_index(x,y,width_,height_)];
          tile.touched += !sample.empty();
          tile.max_layers = std::max(tile.max_layers, sample.size());
          for (auto it=sample.rbegin(); it!=sample.rend(); ++it)
            resolve(x,y,it->first,it->second);
        }
      return;
    }
    auto& records = tile.fragments;
    std::sort(records.begin(), records.end(), [](const Fragment& a, const Fragment& b) {
      if (a.sample != b.sample) return a.sample < b.sample;
      if (a.depth != b.depth) return a.depth > b.depth;
      return a.sequence > b.sequence; // last write wins at equal depth
    });
    std::size_t layers = 0;
    for (std::size_t i=0; i<records.size(); ++i) {
      const auto& f = records[i];
      if (i && f.sample == records[i-1].sample && f.depth == records[i-1].depth) continue;
      if (!i || f.sample != records[i-1].sample) { ++tile.touched; layers = 0; }
      tile.max_layers = std::max(tile.max_layers, ++layers);
      alpha_info value{f.color,vec3(0),vec3(0),vec3(0)};
      std::size_t auxiliary=f.sequence*stride_;
      if(mask_&1) value.normal=tile.auxiliary[auxiliary++];
      if(mask_&2) value.position=tile.auxiliary[auxiliary++];
      if(mask_&4) value.uv=tile.auxiliary[auxiliary++];
      resolve(f.sample/height_, f.sample%height_, f.depth, value);
    }
  }
  template<class Resolve> void resolve(Resolve fn) {
    for (std::size_t i=0; i<size(); ++i) resolve_tile(i, fn);
  }
  std::size_t capacity_bytes() const {
    std::size_t result = tiles_.capacity()*sizeof(Tile);
    for (const auto& tile : tiles_) result += tile.fragments.capacity()*sizeof(Fragment)+tile.auxiliary.capacity()*sizeof(vec3);
    // Reference tree nodes are not included: allocator bookkeeping is unknown.
    return result + reference_.capacity()*sizeof(std::map<Float, alpha_info>);
  }
  void release() {
    for(auto& tile:tiles_) {
      std::vector<Fragment>().swap(tile.fragments);
      std::vector<vec3>().swap(tile.auxiliary);
    }
    std::vector<std::map<Float,alpha_info>>().swap(reference_);
  }
  std::size_t max_layers() const {
    std::size_t result = 0;
    for (const auto& tile : tiles_) result = std::max(result, tile.max_layers);
    return result;
  }
  std::size_t touched_samples() const {
    std::size_t result = 0;
    for (const auto& tile : tiles_) result += tile.touched;
    return result;
  }
};
#endif
