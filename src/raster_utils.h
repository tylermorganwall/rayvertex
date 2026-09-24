#ifndef RAYVERTEX_RASTER_UTILS_H
#define RAYVERTEX_RASTER_UTILS_H

#include <cassert>
#include <cstddef>
#include <limits>
#include <stdexcept>

inline std::size_t checked_samples(int width, int height) {
  if (width <= 0 || height <= 0 ||
      static_cast<std::size_t>(width) > std::numeric_limits<std::size_t>::max() /
                                        static_cast<std::size_t>(height))
    throw std::invalid_argument("Invalid raster dimensions");
  return static_cast<std::size_t>(width) * static_cast<std::size_t>(height);
}

// Legacy transparency layout: y varies fastest. R matrices use a different
// layout. Every triangle/line producer and resolver must use this helper.
inline std::size_t fragment_index(int x, int y, int width, int height) {
  assert(x >= 0 && x < width && y >= 0 && y < height);
  return static_cast<std::size_t>(y) + static_cast<std::size_t>(height) * x;
}

constexpr unsigned int ssao_noise_dimension = 4;
inline unsigned int ssao_noise_index(unsigned int x, unsigned int y) {
  return x % ssao_noise_dimension + ssao_noise_dimension * (y % ssao_noise_dimension);
}

// Source and destination must not alias. Preserve the asymmetric [-2, 1]
// footprint and per-pixel boundary normalization of the original box filter.
inline void blur_ambient_column(const double* source, double* dest, int width, int height,
                                int x, bool reverse = false) {
  assert(source != dest);
  for (int iy = 0; iy < height; ++iy) {
    int y = reverse ? height - iy - 1 : iy;
    double sum = 0;
    int count = 0;
    for (int dx = -2; dx < 2; ++dx) {
      for (int dy = -2; dy < 2; ++dy) {
        int sx = x + dx, sy = y + dy;
        if (sx >= 0 && sx < width && sy >= 0 && sy < height) {
          sum += source[sx + static_cast<std::size_t>(width) * sy];
          ++count;
        }
      }
    }
    dest[x + static_cast<std::size_t>(width) * y] = sum / count;
  }
}

inline void blur_ambient(const double* source, double* dest, int width, int height,
                         bool reverse = false) {
  for (int ix = 0; ix < width; ++ix) {
    int x = reverse ? width - ix - 1 : ix;
    blur_ambient_column(source, dest, width, height, x, reverse);
  }
}
#endif
