#ifndef RAYVERTEX_SHADOW_SAMPLE_H
#define RAYVERTEX_SHADOW_SAMPLE_H
#include <algorithm>
#include <cstddef>

// R matrices use x + width*y. Keep x-outer/y-inner reference accumulation,
// inclusive depth equality, all 25 taps, and clamp each border tap independently.
inline double shadow_pcf25(const double* depth, int width, int height,
                           int i, int j, double threshold, double shadow_intensity) {
  double sum=0;
  if(i>=2 && j>=2 && i<width-2 && j<height-2) {
    for(int x=-2;x<=2;++x) for(int y=-2;y<=2;++y)
      sum += depth[i+x+std::size_t(width)*(j+y)]>threshold ? 1.0 : shadow_intensity;
  } else {
    for(int x=-2;x<=2;++x) for(int y=-2;y<=2;++y) {
      const int xx=std::max(0,std::min(width-1,i+x));
      const int yy=std::max(0,std::min(height-1,j+y));
      sum += depth[xx+std::size_t(width)*yy]>threshold ? 1.0 : shadow_intensity;
    }
  }
  return sum/25;
}
#endif
