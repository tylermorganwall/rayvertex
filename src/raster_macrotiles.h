#ifndef RAYVERTEX_RASTER_MACROTILES_H
#define RAYVERTEX_RASTER_MACROTILES_H

#include <algorithm>
#include <cstddef>
#include <stdexcept>
#include <vector>

// Schedule spatial groups of existing coverage blocks. Primitive bins and
// microblock bounds are unchanged; each active block has exactly one owner.
class RasterMacrotiles {
  std::vector<std::size_t> offsets_, blocks_;
public:
  template<class Active>
  RasterMacrotiles(int columns,int rows,int block_size,int edge,Active active) {
    if(columns<=0 || rows<=0 || block_size<=0 || edge<=0)
      throw std::invalid_argument("Invalid raster macrotile dimensions");
    const int step=1+(edge-1)/block_size;
    for(int mx=0;mx<columns;) {
      const int end_x=mx+std::min(step,columns-mx);
      for(int my=0;my<rows;) {
        const int end_y=my+std::min(step,rows-my);
        const auto start=blocks_.size();
        for(int x=mx;x<end_x;++x) for(int y=my;y<end_y;++y) {
          const std::size_t tile=y+std::size_t(rows)*x;
          if(active(tile)) blocks_.push_back(tile);
        }
        if(start!=blocks_.size()) offsets_.push_back(start);
        my=end_y;
      }
      mx=end_x;
    }
    offsets_.push_back(blocks_.size());
  }
  std::size_t size() const { return offsets_.size()-1; }
  std::size_t begin(std::size_t macro) const { return offsets_[macro]; }
  std::size_t end(std::size_t macro) const { return offsets_[macro+1]; }
  std::size_t block(std::size_t entry) const { return blocks_[entry]; }
  std::size_t capacity_bytes() const {
    return (offsets_.capacity()+blocks_.capacity())*sizeof(std::size_t);
  }
};
#endif
