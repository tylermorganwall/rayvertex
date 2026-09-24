#include "../src/shadow_sample.h"
#include <cassert>
#include <limits>
#include <vector>
#include <iostream>
int main() {
  std::size_t checked=0;
  for(int width=1;width<=17;++width) for(int height=1;height<=19;++height) {
    std::vector<double> depth(width*height);
    for(std::size_t k=0;k<depth.size();++k)
      depth[k]=k%11==0 ? std::numeric_limits<double>::infinity() : (k%23)/23.0;
    for(int i=0;i<width;++i) for(int j=0;j<height;++j)
      for(double threshold:{-0.1,0.0,0.3,0.7,1.0,std::numeric_limits<double>::infinity()})
        for(double intensity:{0.0,0.2,1.0}) {
          double expected=0;
          for(int x=-2;x<=2;++x) for(int y=-2;y<=2;++y) {
            int xx=std::max(0,std::min(width-1,i+x)), yy=std::max(0,std::min(height-1,j+y));
            expected += depth[xx+width*yy]>threshold ? 1.0 : intensity;
          }
          expected/=25;
          assert(shadow_pcf25(depth.data(),width,height,i,j,threshold,intensity)==expected);
          ++checked;
        }
  }
  std::cout<<checked<<" exact PCF comparisons passed\n";
}
