#ifndef RAYVERTEX_COVERAGE_BOUNDS_H
#define RAYVERTEX_COVERAGE_BOUNDS_H

#include <algorithm>
#include <array>
#include <cmath>

enum class RasterBlockCoverage { outside, partial, inside };

// Extrema of the existing multiply-from-origin edge evaluation, not a new
// coverage rule. Each rounded multiply/add is monotone on a finite rectangle;
// evaluating its four endpoints with the same association bounds every sample.
inline RasterBlockCoverage raster_block_coverage(
    const std::array<double,3>& initial, const std::array<double,3>& step_x,
    const std::array<double,3>& step_y, unsigned width,unsigned height,int culling) {
  if(width==0 || height==0) return RasterBlockCoverage::outside;
  bool positive_possible=true, negative_possible=true;
  bool positive_full=true, negative_full=true;
  for(int edge=0;edge<3;++edge) {
    const double x0=initial[edge]+0.0*step_x[edge];
    const double x1=initial[edge]+double(width-1)*step_x[edge];
    const double corners[]={x0+0.0*step_y[edge],x1+0.0*step_y[edge],
      x0+double(height-1)*step_y[edge],x1+double(height-1)*step_y[edge]};
    double lo=corners[0],hi=corners[0];
    for(double value:corners) {
      // Exceptional edge arithmetic falls back to the original sample tests.
      if(!std::isfinite(value)) return RasterBlockCoverage::partial;
      lo=std::min(lo,value); hi=std::max(hi,value);
    }
    positive_possible&=hi>=0; negative_possible&=lo<=0;
    positive_full&=lo>=0; negative_full&=hi<=0;
  }
  if(culling==1) {
    if(!positive_possible) return RasterBlockCoverage::outside;
    if(positive_full) return RasterBlockCoverage::inside;
  } else if(culling==2) {
    if(!negative_possible) return RasterBlockCoverage::outside;
    if(negative_full) return RasterBlockCoverage::inside;
  } else {
    if(!positive_possible && !negative_possible) return RasterBlockCoverage::outside;
    if(positive_full || negative_full) return RasterBlockCoverage::inside;
  }
  return RasterBlockCoverage::partial;
}
#endif
