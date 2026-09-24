#include "experiments/tangent_basis.h"
#include <random>
#include <cassert>
#include <cmath>
#include <cstring>
#include <iostream>
#include <limits>
void compare(vec3 a,vec3 b,vec3 n,vec2 du,vec2 dv) {
  const auto reference=scalar_tangent_basis(a,b,n,du,dv);
  const auto actual=algebraic_tangent_basis(a,b,n,du,dv);
  for(int v=0;v<2;++v) for(int k=0;k<3;++k) {
    const double x=reference[v][k],y=actual[v][k];
    if(std::isnan(x)) assert(std::isnan(y));
    else assert(std::memcmp(&x,&y,sizeof(double))==0);
  }
}
int main() {
  std::mt19937 random(951);
  std::uniform_real_distribution<double> value(-10,10);
  auto vector=[&] { return vec3(value(random),value(random),value(random)); };
  for(int i=0;i<1000000;++i) {
    const double scale=std::pow(10.0,(i%71)-35);
    compare(vector()*scale,vector()*scale,vector(),vec2(value(random),value(random)),
            vec2(value(random),value(random)));
  }
  for(double scale:{0.0,-0.0,1e-300,1e300,std::numeric_limits<double>::infinity(),
                    std::numeric_limits<double>::quiet_NaN()}) {
    compare(vec3(scale,0,0),vec3(0,scale,0),vec3(0,0,1),vec2(1,0),vec2(0,-1));
    compare(vec3(scale),vec3(scale),vec3(scale),vec2(0),vec2(0));
  }
  std::cout<<"1000012 exact tangent basis comparisons (NaNs compared by class) passed\n";
}
