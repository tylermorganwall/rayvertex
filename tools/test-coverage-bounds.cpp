#include "../src/coverage_bounds.h"
#include <cassert>
#include <iostream>
#include <limits>
#include <random>

int main() {
  std::mt19937_64 random(1421);
  std::uniform_real_distribution<double> value(-1,1);
  std::size_t full=0,rejected=0,partial=0,samples=0;
  for(int trial=0;trial<100000;++trial) {
    const unsigned width=1+random()%17,height=1+random()%19;
    const double scale=std::ldexp(1.0,int(random()%2001)-1000);
    std::array<double,3> initial,dx,dy;
    for(int edge=0;edge<3;++edge) {
      initial[edge]=value(random)*scale;
      dx[edge]=value(random)*scale;
      dy[edge]=value(random)*scale;
    }
    if(trial%7==0) initial.fill(0.0);
    if(trial%11==0) dx.fill(-0.0);
    if(trial%13==0) dy.fill(0.0);
    for(int culling:{1,2,3}) {
      auto classification=raster_block_coverage(initial,dx,dy,width,height,culling);
      full+=classification==RasterBlockCoverage::inside;
      rejected+=classification==RasterBlockCoverage::outside;
      partial+=classification==RasterBlockCoverage::partial;
      for(unsigned x=0;x<width;++x) {
        std::array<double,3> row;
        for(int edge=0;edge<3;++edge) row[edge]=initial[edge]+x*dx[edge];
        for(unsigned y=0;y<height;++y) {
          const double a=row[0]+y*dy[0],b=row[1]+y*dy[1],c=row[2]+y*dy[2];
          const bool positive=a>=0 && b>=0 && c>=0,negative=a<=0 && b<=0 && c<=0;
          const bool inside=culling==1 ? positive : culling==2 ? negative : positive || negative;
          assert(classification!=RasterBlockCoverage::inside || inside);
          assert(classification!=RasterBlockCoverage::outside || !inside);
          ++samples;
        }
      }
    }
  }
  const double inf=std::numeric_limits<double>::infinity();
  assert(raster_block_coverage({inf,0,0},{0,0,0},{0,0,0},4,4,3)==RasterBlockCoverage::partial);
  assert(full>0 && rejected>0 && partial>0);
  std::cout << samples << " exact sample classifications, inclusive zeros, extreme scales and conservative exceptional fallback passed\n";
}
