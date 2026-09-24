#include "../src/raster_utils.h"
#include <algorithm>
#include <array>
#include <iostream>
#include <vector>

int main() {
  std::array<int, 16> noise{};
  for (unsigned int x = 0; x < 16; ++x)
    for (unsigned int y = 0; y < 16; ++y)
      ++noise.at(ssao_noise_index(x, y));
  for (int n : noise) assert(n == 16);
  for (auto dims : {std::array<int, 2>{31, 23}, {23, 31}, {1, 1}, {7, 9}}) {
    int w = dims[0], h = dims[1];
    std::vector<int> visited(checked_samples(w, h));
    std::vector<double> source(visited.size()), forward(visited.size()), reverse(visited.size());
    for (std::size_t i = 0; i < source.size(); ++i) source[i] = (i * 7 % 19) / 19.0;
    for (int x = 0; x < w; ++x)
      for (int y = 0; y < h; ++y) ++visited.at(fragment_index(x, y, w, h));
    for (int n : visited) assert(n == 1);
    blur_ambient(source.data(), forward.data(), w, h);
    blur_ambient(source.data(), reverse.data(), w, h, true);
    assert(forward == reverse);
    for (double n : forward) assert(n >= 0 && n <= 1);
  }
  bool threw = false;
  try { checked_samples(-1, 3); } catch (const std::invalid_argument&) { threw = true; }
  assert(threw);
  std::cout << "raster utilities: passed\n";
}
