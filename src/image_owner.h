#ifndef RAYVERTEX_IMAGE_OWNER_H
#define RAYVERTEX_IMAGE_OWNER_H

#include <memory>
#include "stbimageheaders/stb_image.h"

struct StbiDeleter {
  void operator()(float* pixels) const { stbi_image_free(pixels); }
};

// Construction can fail partway through loading a shader's five textures.
// Each completed decode already has an owner when the next decode begins.
class ImageOwner {
  std::unique_ptr<float, StbiDeleter> pixels;
public:
  ImageOwner() = default;
  ImageOwner& operator=(float* value) { pixels.reset(value); return *this; }
  explicit operator bool() const { return pixels != nullptr; }
  operator const float*() const { return pixels.get(); }
};
#endif
