#include "../src/fragment_arena.h"
#include <random>
#include <iostream>

int main() {
  unsetenv("RAYVERTEX_REFERENCE_TRANSPARENCY");
  for (auto dims : {std::pair<int,int>{37,19}, {19,37}, {1,1}}) {
    const int w=dims.first, h=dims.second;
    FragmentArena arena(w,h);
    std::vector<std::map<Float,alpha_info>> expected(checked_samples(w,h));
    std::mt19937 random(123);
    // Many equal depths, negative zero, infinity, and over 129 distinct layers.
    for (int i=0; i<250000; ++i) {
      int x=random()%w, y=random()%h;
      Float z=(random()%1024)/1024.;
      if (i%37==0) z=std::numeric_limits<Float>::infinity();
      if (i%41==0) z=-0.;
      alpha_info value{vec4(i),vec3(i+1),vec3(i+2),vec3(i+3)};
      expected[fragment_index(x,y,w,h)][z]=value;
      arena.insert(x,y,z,value);
    }
    std::size_t count=0, maximum=0;
    arena.resolve([&](int x,int y,Float depth,const alpha_info& value) {
      auto& sample=expected[fragment_index(x,y,w,h)];
      assert(!sample.empty());
      auto winner=std::prev(sample.end());
      assert(depth==winner->first);
      assert(value.color==winner->second.color && value.normal==winner->second.normal);
      assert(value.position==winner->second.position && value.uv==winner->second.uv);
      sample.erase(winner);
      ++count;
    });
    for(const auto& sample:expected) assert(sample.empty());
    assert(count>129 && arena.max_layers()>129);
    assert(arena.touched_samples()==std::size_t(w*h));
  }
  for(unsigned mask=0;mask<8;++mask) {
    FragmentArena arena(7,5,4,mask);
    alpha_info value{vec4(1),vec3(2),vec3(3),vec3(4)};
    arena.insert(3,2,.5,value);
    arena.resolve([&](int x,int y,Float z,const alpha_info& result) {
      assert(x==3 && y==2 && z==.5 && result.color==value.color);
      assert(result.normal==((mask&1) ? value.normal : vec3(0)));
      assert(result.position==((mask&2) ? value.position : vec3(0)));
      assert(result.uv==((mask&4) ? value.uv : vec3(0)));
    });
    const auto bytes=arena.capacity_bytes();
    arena.release();
    assert(arena.capacity_bytes()<bytes && arena.max_layers()==1);
  }
  FragmentArena empty(800,800);
  assert(empty.capacity_bytes()<800*800*sizeof(std::map<Float,alpha_info>));
  bool threw=false;
  try { FragmentArena invalid(1,1,0); } catch(const std::invalid_argument&) { threw=true; }
  assert(threw);
  std::cout << "fragment arena: finite/infinite depth, exact ties, auxiliary values, rectangular dimensions, growth and empty storage passed\n";
}
