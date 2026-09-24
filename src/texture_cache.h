#ifndef RAYVERTEX_TEXTURE_CACHE_H
#define RAYVERTEX_TEXTURE_CACHE_H

#include <memory>
#include <string>
#include <unordered_map>
#include <cstdlib>
#include "Rcpp.h"
#include "image_owner.h"

struct CachedTexture {
  std::shared_ptr<float> pixels;
  int width = 0, height = 0, channels = 0;
};

// Frame-local, main-thread-only decode cache. All material decodes use original
// channel count, float storage and the renderer's gamma=1 setting. If another
// decode/color-space option is introduced, it must become part of this key.
class TextureCache {
  const bool enabled;
  const TextureCache* parent;
  std::unordered_map<std::string, std::string> identities;
  std::unordered_map<std::string, CachedTexture> textures;
public:
  explicit TextureCache(const TextureCache* snapshot=nullptr, bool immutable=false)
    : enabled(immutable || std::getenv("RAYVERTEX_REFERENCE_ASSETS")==nullptr), parent(snapshot) {}
  std::size_t hits=0;
  std::size_t decodes = 0, payload_bytes = 0;
  std::shared_ptr<float> load(const char* filename, int* width, int* height, int* channels) {
    // Prepared paths are canonical. A hit never consults a changed source file.
    if(parent) {
      auto found=parent->textures.find(filename);
      if(found!=parent->textures.end()) {
        ++hits;
        *width=found->second.width; *height=found->second.height; *channels=found->second.channels;
        return found->second.pixels;
      }
    }
    if (!enabled) {
      *width = *height = *channels = 0;
      std::shared_ptr<float> pixels(stbi_loadf(filename, width, height, channels, 0), StbiDeleter());
      if (pixels) {
        ++decodes;
        payload_bytes += static_cast<std::size_t>(*width) * *height * *channels * sizeof(float);
      }
      return pixels;
    }
    std::string path(filename);
    auto identity = identities.find(path);
    if (identity == identities.end()) {
      Rcpp::Function normalize_path = Rcpp::Environment::base_env()["normalizePath"];
      std::string canonical = Rcpp::as<std::string>(normalize_path(path,
        Rcpp::Named("winslash") = "/", Rcpp::Named("mustWork") = false));
      identity = identities.emplace(path, canonical).first;
    }
    auto found = textures.find(identity->second);
    if (found == textures.end()) {
      CachedTexture texture;
      texture.pixels.reset(stbi_loadf(filename, &texture.width, &texture.height,
                                     &texture.channels, 0), StbiDeleter());
      if (!texture.pixels) {
        *width = *height = *channels = 0;
        return {};
      }
      ++decodes;
      payload_bytes += static_cast<std::size_t>(texture.width) * texture.height *
                       texture.channels * sizeof(float);
      found = textures.emplace(identity->second, std::move(texture)).first;
    }
    *width = found->second.width;
    *height = found->second.height;
    *channels = found->second.channels;
    return found->second.pixels;
  }
};
#endif
