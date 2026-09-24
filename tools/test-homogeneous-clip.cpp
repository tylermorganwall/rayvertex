#include "../src/triangle_setup.h"
#include <iostream>
#include "../src/light.h"
int main() {
  const std::array<vec4,3> input={vec4(2,2,.5,1),vec4(8,2,.5,1),vec4(2,8,.5,1)};
  auto unchanged=clip_homogeneous(input,11,11);
  assert(!unchanged.changed && unchanged.size==3);
  for(int i=0;i<3;++i) assert(unchanged.vertices[i].position==input[i]);
  for(int plane=0;plane<7;++plane) {
    auto triangle=input;
    switch(plane) {
      case 0: triangle[0].x=-5; break;
      case 1: triangle[0].x=20; break;
      case 2: triangle[0].y=-5; break;
      case 3: triangle[0].y=20; break;
      case 4: triangle[0].z=-1; break;
      case 5: triangle[0].z=2; break;
      default: triangle[0]=vec4(-2,-2,-.5,-1);
    }
    auto clipped=clip_homogeneous(triangle,11,11);
    assert(clipped.changed && clipped.size>=3);
    for(std::size_t i=0;i<clipped.size;++i) {
      auto p=clipped.vertices[i].position;
      assert(p.w>0 && p.x>=-1e-14 && p.x<=10*p.w+1e-14);
      assert(p.y>=-1e-14 && p.y<=10*p.w+1e-14 && p.z>=-1e-14 && p.z<=p.w+1e-14);
      auto bc=clipped.vertices[i].weights;
      assert(std::abs(bc.x+bc.y+bc.z-1)<1e-14);
      auto reconstructed=triangle[0]*bc.x+triangle[1]*bc.y+triangle[2]*bc.z;
      assert(glm::length(reconstructed-p)<1e-13);
    }
  }
  bool threw=false;
  try { auto bad=input; bad[0].x=std::numeric_limits<Float>::quiet_NaN(); clip_homogeneous(bad,11,11); }
  catch(const std::invalid_argument&) { threw=true; }
  assert(threw);
  TriangleBins bins(11,11,4);
  bins.add_clipped({vec4(-1e100,2,.5,1),input[1],input[2]},0,0,3,false);
  bins.build();
  assert(bins.input_primitives==1 && bins.triangles.size()>=1);
  for(auto direction:{vec3(0,0,1),glm::normalize(vec3(1,2,3))}) {
    DirectionalLight light(direction,vec3(1),vec3(0),vec3(0,1,0),4,0.1,10,Mat(1),Mat(1),Mat(1),1);
    for(int x:{-1,1}) for(int y:{-1,1}) for(int z:{-1,1}) {
      auto p=light.M*vec4(x,y,z,1);
      assert(p.z/p.w>=-1 && p.z/p.w<=1);
    }
    assert(light.shadow_bias_scale>0 && light.shadow_bias_scale<1);
  }
  std::cout << "homogeneous clipping: all planes, w crossing, identity, attributes, nonfinite and huge bounds passed\n";
}
