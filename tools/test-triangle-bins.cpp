#include "../src/triangle_setup.h"
#include <iostream>
int main() {
  TriangleBins bins(17,13,4);
  const std::array<vec4,3> triangle={vec4(0,0,.5,1),vec4(17,0,.5,1),vec4(0,13,.5,1)};
  for(int i=0;i<100;++i) bins.add(triangle,i,i%3,3,false);
  bins.build();
  assert(bins.attempted==100 && bins.culled==0);
  assert(bins.references()==bins.size()*100);
  for(std::size_t tile=0;tile<bins.size();++tile) {
    assert(bins.active(tile));
    for(std::size_t entry=bins.begin(tile);entry<bins.end(tile);++entry)
      assert(bins.at(entry).face==int(entry-bins.begin(tile)));
  }
  TriangleBins empty(1,1,4);
  empty.add({vec4(0,0,0,1),vec4(0,0,0,1),vec4(0,0,0,1)},0,0,1,false);
  empty.build();
  assert(!empty.active(0));
  std::cout << "triangle bins: single setup, arbitrary overlap, exact submission order, odd dimensions and culling passed\n";
}
