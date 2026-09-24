#include "../src/raster_macrotiles.h"
#include <cassert>
#include <iostream>

int main() {
  for(int columns:{1,3,19,71}) for(int rows:{1,14,37})
    for(int block:{1,4,7}) for(int edge:{16,32,64}) for(int mode:{0,1,2}) {
      auto active=[&](std::size_t tile) { return mode==1 || (mode==2 && tile%5==0); };
      RasterMacrotiles macros(columns,rows,block,edge,active);
      std::vector<int> visited(std::size_t(columns)*rows,0);
      const int step=1+(edge-1)/block;
      for(std::size_t macro=0;macro<macros.size();++macro) {
        assert(macros.begin(macro)<macros.end(macro));
        const auto first=macros.block(macros.begin(macro));
        const auto mx=(first/rows)/step, my=(first%rows)/step;
        for(std::size_t i=macros.begin(macro);i<macros.end(macro);++i) {
          const auto tile=macros.block(i);
          assert(tile<visited.size());
          assert((tile/rows)/step==mx && (tile%rows)/step==my);
          ++visited[tile];
        }
      }
      for(std::size_t tile=0;tile<visited.size();++tile)
        assert(visited[tile]==int(active(tile)));
    }
  bool failed=false;
  try { RasterMacrotiles bad(1,1,0,16,[](std::size_t) { return true; }); }
  catch(const std::invalid_argument&) { failed=true; }
  assert(failed);
  std::cout << "spatial macrotiles: complete disjoint active-block ownership, empty regions, rectangular/partial bounds and arbitrary microblocks passed\n";
}
