#include "../src/rayimage.h"
#include <cassert>
#include <cmath>
#include <iostream>

int main() {
  float gray[] = {2.0f};
  float gray_alpha[] = {2.0f, 0.25f};
  float rgb[] = {2.0f, 0.5f, 0.75f};
  float rgba[] = {2.0f, 0.5f, 0.75f, 0.25f};
  for (double u : {-2.5, -1.0, 0.0, 0.5, 1.0, 2.5}) {
    for (double v : {-1.5, 0.0, 0.9, 1.5}) {
      assert(trivalue(u, v, gray, 1, 1, 1) == vec4(2, 2, 2, 1));
      assert(trivalue(u, v, gray_alpha, 1, 1, 2) == vec4(2, 2, 2, 0.25));
      assert(trivalue(u, v, rgb, 1, 1, 3) == vec4(2, 0.5, 0.75, 1));
      assert(trivalue(u, v, rgba, 1, 1, 4) == vec4(2, 0.5, 0.75, 0.25));
      assert(trivalue(u, v, reflection_map_info{gray, 1, 1, 1}) == vec4(2, 2, 2, 1));
    }
  }
  float narrow[] = {0.0f, 0.0f, 2.0f, 1.0f};
  assert(trivalue(0, 0.5, narrow, 1, 2, 2) == vec4(1, 1, 1, 0.5));
  assert(trivalue(0, 0, narrow, 1, 2, 2) == vec4(2, 2, 2, 1));
  std::cout << "texture sampling: passed\n";
}
