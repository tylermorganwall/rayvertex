#include "../src/light.h"
#include <random>
#include <cassert>
#include <cmath>
#include <iostream>
int main() {
  std::mt19937 random(74);
  std::uniform_real_distribution<double> value(-1000,1000);
  for(int i=0;i<100000;++i) {
    Light light({value(random),value(random),value(random)}, {0.2,0.6,1.1},1,0.2,0.001,0.8);
    vec3 p(value(random),value(random),value(random));
    auto sample=light.sample(p);
    assert(sample.direction==light.CalcLightDir(p));
    assert(sample.attenuation==light.CalcPointLightAtten(p));
  }
  Light coincident({0,0,0},{1,1,1},1,0,0,1);
  const auto sample=coincident.sample({0,0,0});
  for(int i=0;i<3;++i) assert(std::isnan(sample.direction[i]) && std::isnan(coincident.CalcLightDir({0,0,0})[i]));
  assert(sample.attenuation==coincident.CalcPointLightAtten({0,0,0}));
  std::cout<<"100000 exact point-light samples and legacy zero-distance fallback passed\n";
}
