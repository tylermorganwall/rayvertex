#include "raster_utils.h"
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_RESIZE_IMPLEMENTATION

#ifndef RAYRASTERH
#define RAYRASTERH

#include "Rcpp.h"

#ifndef RAYVERTEX_NO_THREADS
#ifdef __EMSCRIPTEN__
  #ifdef __EMSCRIPTEN_PTHREADS__
    #define HAVE_THREADS
  #else
  #endif
#else
  #define HAVE_THREADS
#endif 
#endif


#define FLOAT_AS_DOUBLE
// #ifndef FLOAT_AS_DOUBLE
// typedef float Float;
// #else 
// typedef double Float;
// #endif

#include <vector>
#include <cstdint>
#include <limits>
#include <cmath>
#include <functional>
#include <algorithm>
#include <utility>
#include "stbimageheaders/stb_image.h"
#undef STB_IMAGE_IMPLEMENTATION
#include "stbimageheaders/stb_image_resize2.h"
#include <memory>
#include <map>
#include "glm.hpp"
#include "gtc/matrix_transform.hpp"
#include "defines.h"
#include "filltri.h"
#include "raster_profile.h"

#include "shaders.h"
#include "rayimage.h"
#include "model.h"
#include "single_sample.h"
// [[Rcpp::depends(RcppThread)]]
#include "RcppThread.h"
#include "dummythreadpool.h"
#include "raster_scheduler.h"
#include "texture_cache.h"
#include "prepared_scene.h"
#include "screen_view.h"

#include "material.h"

#include "light.h"
#include "line.h"
#include "GBuffer.h"

using namespace Rcpp;

inline stbir_pixel_layout stbir_layout_from_channels(int channels) {
  switch(channels) {
  case 1:
    return STBIR_1CHANNEL;
  case 2:
    return STBIR_2CHANNEL;
  case 3:
    return STBIR_RGB;
  case 4:
    return STBIR_4CHANNEL;
  default:
    throw std::runtime_error("Reflection map must have between 1 and 4 channels");
  }
}

inline void resize_reflection_map(const float* input_pixels, int input_w, int input_h,
                                  float* output_pixels, int output_w, int output_h,
                                  int channels) {
  void* resize_result = stbir_resize(input_pixels, input_w, input_h, 0,
                                     output_pixels, output_w, output_h, 0,
                                     stbir_layout_from_channels(channels),
                                     STBIR_TYPE_FLOAT, STBIR_EDGE_WRAP,
                                     STBIR_FILTER_CUBICBSPLINE);
  if(resize_result == nullptr) {
    throw std::runtime_error("Reflection map resizing failed");
  }
}

inline vec3 clamp(const vec3& c, Float clamplow, Float clamphigh) {
  vec3 temp = c;
  if(c[0] > clamphigh) {
    temp[0] = clamphigh;
  } else if(c[0] < clamplow) {
    temp[0] = clamplow;
  }
  if(c[1] > clamphigh) {
    temp[1] = clamphigh;
  } else if(c[1] < clamplow) {
    temp[1] = clamplow;
  }
  if(c[2] > clamphigh) {
    temp[2] = clamphigh;
  } else if(c[2] < clamplow) {
    temp[2] = clamplow;
  }
  return(temp);
}

inline vec4 clamp(const vec4& c, Float clamplow, Float clamphigh) {
  vec4 temp = c;
  if(c[0] > clamphigh) {
    temp[0] = clamphigh;
  } else if(c[0] < clamplow) {
    temp[0] = clamplow;
  }
  if(c[1] > clamphigh) {
    temp[1] = clamphigh;
  } else if(c[1] < clamplow) {
    temp[1] = clamplow;
  }
  if(c[2] > clamphigh) {
    temp[2] = clamphigh;
  } else if(c[2] < clamplow) {
    temp[2] = clamplow;
  }
  if(c[3] > clamphigh) {
    temp[3] = clamphigh;
  } else if(c[3] < clamplow) {
    temp[3] = clamplow;
  }
  return(temp);
}

template<class T>
inline T lerp(Float t, T v1, T v2) {
  return((1-t) * v1 + t * v2);
}

struct JFASeed {
  int x;
  int y;
};
template<class Pool>
static void
apply_toon_outlines_jfa(Pool& pool, int workers, std::vector<vec3> &color_buffer,
                        const std::vector<OutlineGBufferPixel> &gbuffer,
                        int width, int height, Float fov_y,
                        Float ortho_view_height,
						NumericMatrix &zbuffer,
                        NumericMatrix &linear_depth) {
  if (width <= 1 || height <= 1) {
    return;
  }
  const std::size_t n =
      static_cast<std::size_t>(width) * static_cast<std::size_t>(height);
  if (color_buffer.size() != n || gbuffer.size() != n) {
    return;
  }

  std::vector<JFASeed> seeds_curr(n);
  std::vector<JFASeed> seeds_next(n);

  dispatch_screen_rows(pool, workers, height, [&](int y) {
    for (int x = 0; x < width; ++x) {
      std::size_t idx = static_cast<std::size_t>(y) * width + x;
      const OutlineGBufferPixel &px = gbuffer[idx];

      // Default value
      seeds_curr[idx].x = -1;
      seeds_curr[idx].y = -1;

      if (px.outline_width <= 0.0f || std::isinf(px.depth_view)) {
        continue;
      }

      bool is_edge = false;

      // Check 8-neighborhood for a discontinuity in occupancy/material
      for (int dy = -1; dy <= 1 && !is_edge; ++dy) {
        for (int dx = -1; dx <= 1 && !is_edge; ++dx) {
          if (dx == 0 && dy == 0) {
            continue;
          }
          int nx = x + dx;
          int ny = y + dy;
          if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
            continue;
          }
          std::size_t nidx = static_cast<std::size_t>(ny) * width + nx;
          const OutlineGBufferPixel &npx = gbuffer[nidx];

          bool n_has_geom = !std::isinf(npx.depth_view);

          // Edge if neighbor has no geometry, or different material
          if (!n_has_geom || npx.material_id != px.material_id) {
            is_edge = true;
            break;
          }
        }
      }

      if (is_edge) {
        seeds_curr[idx].x = x;
        seeds_curr[idx].y = y;
      }
    }
  });

  // Jump Flood
  int max_dim = std::max(width, height);
  int step = 1;
  while (step < max_dim) {
    step <<= 1;
  }
  step >>= 1;

  static const int dirs[9][2] = {
      {-1, -1}, {0, -1}, {1, -1},
      {-1,  0}, {0,  0}, {1,  0},
      {-1,  1}, {0,  1}, {1,  1}
  };

  for (; step >= 1; step >>= 1) {
    dispatch_screen_rows(pool, workers, height, [&](int y) {
      for (int x = 0; x < width; ++x) {
        std::size_t idx = static_cast<std::size_t>(y) * width + x;

        JFASeed best = seeds_curr[idx];
        Float best_dist2 = std::numeric_limits<Float>::infinity();
        if (best.x >= 0) {
          Float dx = (Float)(best.x - x);
          Float dy = (Float)(best.y - y);
          best_dist2 = dx * dx + dy * dy;
        }

        for (int k = 0; k < 9; ++k) {
          int nx = x + dirs[k][0] * step;
          int ny = y + dirs[k][1] * step;
          if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
            continue;
          }
          std::size_t nidx = static_cast<std::size_t>(ny) * width + nx;
          const JFASeed &cand = seeds_curr[nidx];
          if (cand.x < 0) {
            continue;
          }
          Float dx = (Float)(cand.x - x);
          Float dy = (Float)(cand.y - y);
          Float d2 = dx * dx + dy * dy;
          if (d2 < best_dist2) {
            best_dist2 = d2;
            best = cand;
          }
        }

        seeds_next[idx] = best;
      }
    });
    seeds_curr.swap(seeds_next);
  }

  // Apply outlines
  // Seed samples belong to their own toon material and are never overwritten
  // below. Their depth remains immutable while disjoint destinations update.
  dispatch_screen_rows(pool,workers,height,[&](int y) {
    for (int x = 0; x < width; ++x) {
      std::size_t idx = static_cast<std::size_t>(y) * width + x;
      const JFASeed &s = seeds_curr[idx];

      if (s.x < 0) {
        continue; // no edge reachable
      }

      std::size_t sidx = static_cast<std::size_t>(s.y) * width + s.x;
      const OutlineGBufferPixel &seed_px = gbuffer[sidx];
      const OutlineGBufferPixel &dst_px = gbuffer[idx];

      Float radius_px = seed_px.outline_width;
      if (radius_px <= 0.5f) {
        continue;
      }

      Float dx = (Float)(x - s.x);
      Float dy = (Float)(y - s.y);
      Float dist2 = dx * dx + dy * dy;

      if (dist2 > radius_px * radius_px) {
        continue;
      }

      bool dst_has_geom = !std::isinf(dst_px.depth_view);

      // Never draw on the same toon surface itself.
      // Any pixel that originally belonged to the same toon material
      // (has outline_width > 0) is considered "interior" for the purpose
      // of outline drawing, regardless of its depth.
      bool same_toon_surface = dst_has_geom && dst_px.outline_width > 0.0f &&
                               dst_px.material_id == seed_px.material_id;

      if (same_toon_surface) {
        continue;
      }

      // Occluder check: if there is geometry in front of the edge,
      // don't draw the outline through it.
      if (dst_has_geom && dst_px.depth_view < seed_px.depth_view) {
        continue;
      }

      color_buffer[idx] = seed_px.outline_color;
      int sx = s.x;
      int sy = s.y;

      zbuffer(x, y) = zbuffer(sx, sy);
      linear_depth(x, y) = seed_px.depth_view;
    }
  });
}

// [[Rcpp::export]]
List rasterize(List mesh, 
               NumericMatrix lightinfo,
               NumericMatrix line_mat,
               int nx, int ny,
               NumericVector model_color,
               NumericVector lookfrom,
               NumericVector lookat,
               double fov,
               IntegerVector typevals,
               bool has_shadow_map,
               bool calc_ambient, 
               bool tbn,
               double ambient_radius,
               double shadow_map_bias,
               int numbercores,
               int max_indices,
               LogicalVector has_normals_vec,
               LogicalVector has_tex_vec,
               LogicalVector has_texture,  
               LogicalVector has_ambient_texture,
               LogicalVector has_bump_texture,
               LogicalVector has_normal_texture,
               LogicalVector has_specular_texture,
               LogicalVector has_emissive_texture,
               int block_size,
               bool use_default_material,
               double near_clip,
               double  far_clip,
               double shadow_map_intensity,
               NumericVector bounds,
               IntegerVector shadowdims,
               NumericVector camera_up,
               double alpha_line, double line_offset,
               NumericVector ortho_dims, LogicalVector is_dir_light,
               bool aa_lines, LogicalVector &has_vertex_tex, LogicalVector &has_vertex_normals,
               LogicalVector has_reflection_map, Rcpp::String reflection_map_file, 
               double background_sharpness,
               LogicalVector has_refraction, bool environment_map_hdr,
               bool has_environment_map, NumericVector bg_color,
               bool transparent_background,
               bool verbose, int output_mask = 31, SEXP prepared_scene = R_NilValue) {
  RasterProfile profile;
  const TextureCache* prepared_assets=prepared_scene==R_NilValue ? nullptr : &prepared_scene_owner(prepared_scene).textures;
  TextureCache texture_cache(prepared_assets);
  const bool reference_buffers=std::getenv("RAYVERTEX_REFERENCE_BUFFERS")!=nullptr;
  if(reference_buffers) output_mask=31;
  if(output_mask<0 || output_mask>31) throw std::invalid_argument("Invalid raster output mask");
  const bool reference_scheduler = std::getenv("RAYVERTEX_REFERENCE_SCHEDULER") != nullptr;
  const std::size_t batch_size = raster_batch_size();
  const bool visibility=std::getenv("RAYVERTEX_VISIBILITY")!=nullptr;
  List materials = as<List>(mesh["materials"]);
  int number_materials = materials.size();
  struct FrameRequirements { bool shadows = false, outlines = false; } requirements;
  requirements.shadows = has_shadow_map && is_true(any(is_dir_light));
  for(int i = 0; i < number_materials; ++i) {
    List material = materials[i];
    requirements.outlines |= (typevals[i] == 9 || typevals[i] == 10) &&
      as<double>(material["toon_outline_width"]) > 0;
  }
  has_shadow_map = requirements.shadows;
  const bool need_normals=(output_mask&1) || calc_ambient || requirements.outlines;
  const bool need_positions=(output_mask&2) || calc_ambient;
  const bool need_uv=(output_mask&4);
  const bool need_linear_depth=(output_mask&8) || requirements.outlines;
  const bool need_ambient=(output_mask&16) || calc_ambient;
  const unsigned fragment_mask=unsigned(need_normals)+2*unsigned(need_positions)+4*unsigned(need_uv);
  
  Environment pkg = Environment::namespace_env("rayvertex");
  
  Function print_time = pkg["print_time"];
  
  //Turn off gamma correction or get seams in textures/normal maps
  stbi_ldr_to_hdr_gamma(1.0f);
  
  //Resize reflection map for different roughness materials
  std::unique_ptr<float, StbiDeleter> environment_owner;
  float* reflection_map_data = nullptr;
  int nx_r = 0, ny_r = 0, nn_r = 0;
  std::vector<reflection_map_info> reflection_maps;
  reflection_map_info main_reflection_map;
  std::map<std::pair<int,int>,std::unique_ptr<float[]>> reflection_variants;
  std::size_t environment_variant_bytes=0, environment_variant_requests=0;
  
  
  if(has_environment_map) {
    environment_owner.reset(stbi_loadf(reflection_map_file.get_cstring(), &nx_r, &ny_r, &nn_r, 0));
    reflection_map_data = environment_owner.get();
    if(!reflection_map_data || nx_r == 0 || ny_r == 0 || nn_r == 0) {
      throw std::runtime_error("Reflection map loading failed");
    }
    if(environment_map_hdr) {
      float gamma_exp = 1.0/2.2f;
      for(int j = 0; j < nx_r * ny_r * nn_r; j++) {
        reflection_map_data[j] = powf(reflection_map_data[j],gamma_exp);
      }
    }
    main_reflection_map.reflection = reflection_map_data;
    main_reflection_map.nx = nx_r;
    main_reflection_map.ny = ny_r;
    main_reflection_map.nn = nn_r;
  }
  
  // One immutable source, with the exact existing down/up resize per unique
  // dimension pair. Background blur gets its own variant and never mutates a
  // source shared by reflective or refractive materials.
  auto environment_variant=[&](double sharpness, bool blur) -> reflection_map_info {
    ++environment_variant_requests;
    if(!blur) return main_reflection_map;
    const int width=std::max(1,int(double(nx_r)*sharpness));
    const int height=std::max(1,int(double(ny_r)*sharpness));
    const auto key=std::make_pair(width,height);
    auto found=reflection_variants.find(key);
    if(found==reflection_variants.end()) {
      auto pixels=std::make_unique<float[]>(checked_samples(nx_r,ny_r)*nn_r);
      auto scratch=std::make_unique<float[]>(checked_samples(width,height)*nn_r);
      resize_reflection_map(reflection_map_data,nx_r,ny_r,scratch.get(),width,height,nn_r);
      resize_reflection_map(scratch.get(),width,height,pixels.get(),nx_r,ny_r,nn_r);
      environment_variant_bytes+=checked_samples(nx_r,ny_r)*nn_r*sizeof(float);
      found=reflection_variants.emplace(key,std::move(pixels)).first;
    }
    return {found->second.get(),nx_r,ny_r,nn_r};
  };
  for(unsigned int i=0;i<has_reflection_map.size();++i) {
    if(has_reflection_map(i) || has_refraction(i)) {
      List single_material=materials(i);
      double sharpness=as<double>(single_material["reflection_sharpness"]);
      reflection_maps.push_back(environment_variant(sharpness,sharpness<1.0 && sharpness>0.0));
    } else reflection_maps.push_back({nullptr,nx_r,ny_r,nn_r});
  }
  profile.mark("environment_decode");
  print_time(verbose, "Loaded environment maps" );
  
  //Convert R vectors to vec3
  vec3 eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));    //lookat
  vec3 cam_up = vec3(camera_up(0),camera_up(1),camera_up(2));
  vec3 color(model_color(0),model_color(1),model_color(2));

  //Account for colinear camera direction/cam_up vectors
  if(glm::length(glm::cross(eye-center,cam_up)) == 0) {
    const std::string warn_string("Look direction and camera up colinear--using c(0,0,1) for up.");
    Rcpp::warning(warn_string);
    cam_up = vec3(0.,0.,1.f);
  }
  
  Float dist_to_focus = glm::length(eye-center)+0.5;
  vec3 sceneboundmin = vec3(bounds(0),bounds(1),bounds(2));
  vec3 sceneboundmax = vec3(bounds(3),bounds(4),bounds(5));
  Float scene_diag = glm::length(sceneboundmax-sceneboundmin)+0.5;
  vec3 scene_center = (sceneboundmax+sceneboundmin)/(Float)2.0;
  
  far_clip = scene_diag + dist_to_focus;
  
    
  //Generate MVP matrices
  Mat View       = glm::lookAt(eye, center, cam_up);
  Mat Model      = glm::translate(Mat(1.0), vec3(0.0, 0.0, 0.0));
  Mat Projection = fov != 0.0 ? glm::perspective(glm::radians((Float)fov), 
                                    (Float)nx / (Float)ny, 
                                    (Float)near_clip, 
                                    (Float)far_clip) :
    glm::ortho(-(Float)ortho_dims(0)/2, (Float)ortho_dims(0)/2, -(Float)ortho_dims(1)/2, (Float)ortho_dims(1)/2, (Float)near_clip, (Float)far_clip);
  vec4 viewport(0.0f, 0.0f, (Float)nx-1, (Float)ny-1);
  vec4 viewport_depth(0.0f, 0.0f, (Float)shadowdims(0)-1, (Float)shadowdims(1)-1);
  int nx_d = shadowdims(0);
  int ny_d = shadowdims(1);

  
  Mat vp = glm::scale(glm::translate(Mat(1.0f),
                      vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)),
                      vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  Mat vp_shadow = glm::scale(glm::translate(Mat(1.0f),
                             vec3(viewport_depth[2]/2.0f,viewport_depth[3]/2.0f,1.0f/2.0f)),
                             vec3(viewport_depth[2]/2.0f,viewport_depth[3]/2.0f,1.0f/2.0f));
  //Initialize output matrices
  NumericMatrix r(nx,ny);
  NumericMatrix g(nx,ny);
  NumericMatrix b(nx,ny);
  NumericMatrix a(nx,ny);
  
  
  //Fill bg color
  std::fill(r.begin(), r.end(), bg_color[0]) ;
  std::fill(g.begin(), g.end(), bg_color[1] ) ;
  std::fill(b.begin(), b.end(), bg_color[2] ) ;
  float default_alpha = !transparent_background ? 1.0f : 0.0f;
  std::fill(a.begin(), a.end(), default_alpha ) ;
  
  
  //Create buffers
  rayimage image(r,g,b,a, nx,ny);
  
  //Depth buffer
  NumericMatrix zbuffer(nx,ny);
  NumericMatrix zbuffer_depth(has_shadow_map ? shadowdims(0) : 0,
                              has_shadow_map ? shadowdims(1) : 0);
  
  NumericMatrix abuffer(need_ambient ? nx : 0,need_ambient ? ny : 0);
  
  //Fill ambient occlusion buffer
  std::fill(abuffer.begin(), abuffer.end(), 1.0f ) ;
  
  
  //Position space buffer
  NumericMatrix xxbuffer(need_positions ? nx : 0,need_positions ? ny : 0);
  NumericMatrix yybuffer(need_positions ? nx : 0,need_positions ? ny : 0);
  NumericMatrix zzbuffer(need_positions ? nx : 0,need_positions ? ny : 0);

  // Material ID buffer (topmost visible material index per pixel)
  IntegerMatrix material_id_buffer(requirements.outlines ? nx : 0, requirements.outlines ? ny : 0);
  std::fill(material_id_buffer.begin(), material_id_buffer.end(), -1);
  
  
  //Normal space buffer
  NumericMatrix nxbuffer(need_normals ? nx : 0,need_normals ? ny : 0);
  NumericMatrix nybuffer(need_normals ? nx : 0,need_normals ? ny : 0);
  NumericMatrix nzbuffer(need_normals ? nx : 0,need_normals ? ny : 0);
  
  //UV buffer
  NumericMatrix uvxbuffer(need_uv ? nx : 0,need_uv ? ny : 0);
  NumericMatrix uvybuffer(need_uv ? nx : 0,need_uv ? ny : 0);
  NumericMatrix uvzbuffer(need_uv ? nx : 0,need_uv ? ny : 0);

  //Initialize rayimage buffers
  rayimage ambientbuffer(abuffer, nx, ny);
  rayimage positionbuffer(xxbuffer,yybuffer,zzbuffer,nx,ny);
  rayimage normalbuffer(nxbuffer,nybuffer,nzbuffer,nx,ny);
  rayimage uvbuffer(uvxbuffer,uvybuffer,uvzbuffer,nx,ny);
  
  //Initialize zbuffer
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<Float>::infinity() ) ;
  std::fill(zbuffer_depth.begin(), zbuffer_depth.end(), std::numeric_limits<Float>::infinity() ) ;
  profile.mark("frame_allocate_clear");
  print_time(verbose, "Initialized buffers" );
  
  //Initialize Shadow Map bounds and orientation
  //If changed to 0.1-100.0 doesn't work anymore
  Float near_plane = 0.1, far_plane = 10.0;
  // Float near_plane = 0.1f, far_plane = 100.0f;
  
  Mat shadow_inv = glm::inverse(vp * Projection * View * Model);

  std::vector<Light> point_lights;
  for(int i = 0; i < lightinfo.nrow(); i++) {
    if(!is_dir_light(i)) {
      vec3 light_position = View * Model * vec4(lightinfo(i,0),lightinfo(i,1),lightinfo(i,2),1.0);
      point_lights.push_back(Light(light_position,
                                   vec3(lightinfo(i,3),lightinfo(i,4),lightinfo(i,5)),
                                   lightinfo(i,6),lightinfo(i,7),lightinfo(i,8),
                                   lightinfo(i,9)));
    }
  }
  
  std::vector<rayimage > shadowbuffers;
  std::vector<Rcpp::NumericMatrix> shadowbuffer_mats;
  
  std::vector<DirectionalLight> directional_lights;
  for(unsigned int i = 0; i < is_dir_light.length(); i++) {
    if(is_dir_light(i)) {
      if(has_shadow_map) {
        shadowbuffer_mats.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
        std::fill(shadowbuffer_mats.back().begin(), shadowbuffer_mats.back().end(),
                  std::numeric_limits<double>::infinity() ) ;

        rayimage shadowbuffer_temp(shadowbuffer_mats.back(),shadowdims(0),shadowdims(1),shadow_map_intensity);

        shadowbuffers.push_back(shadowbuffer_temp);

      }
      vec3 light_dir_temp = glm::normalize(vec3(lightinfo(i,0),lightinfo(i,1),lightinfo(i,2)));
      vec3 light_up_dir = vec3(0.,1.,0.);
      if(glm::length(glm::cross(light_dir_temp,light_up_dir)) == 0) {
        light_up_dir = vec3(0.f,0.f,1.0f);
      }
      directional_lights.push_back(DirectionalLight(light_dir_temp,
                                                    vec3(lightinfo(i,3),lightinfo(i,4),lightinfo(i,5)),
                                                    scene_center, light_up_dir, scene_diag,
                                                    near_plane, far_plane,
                                                    vp_shadow, Model, shadow_inv,
                                                    lightinfo(i,9)));
      directional_lights.back().view_direction=vec3((View * Model) * vec4(light_dir_temp,0.0));
    }
  }
  profile.mark("lights_shadow_allocate");
  print_time(verbose, "Initialized shadowmaps" );
  
  
  std::vector<rayimage > transparency_buffers;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_r;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_g;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_b;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_a;
  
  for(int i = 0; i < is_dir_light.length(); i++) {
    if(has_shadow_map && is_dir_light(i)) {
      transparency_buffer_mats_r.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
      transparency_buffer_mats_g.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
      transparency_buffer_mats_b.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
      transparency_buffer_mats_a.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
      
      std::fill(transparency_buffer_mats_r.back().begin(), transparency_buffer_mats_r.back().end(), 1.0) ;
      std::fill(transparency_buffer_mats_g.back().begin(), transparency_buffer_mats_g.back().end(), 1.0) ;
      std::fill(transparency_buffer_mats_b.back().begin(), transparency_buffer_mats_b.back().end(), 1.0) ;
      std::fill(transparency_buffer_mats_a.back().begin(), transparency_buffer_mats_a.back().end(), 0.0) ;
      
      rayimage trans_buffer_temp(transparency_buffer_mats_r.back(),
                                 transparency_buffer_mats_g.back(),
                                 transparency_buffer_mats_b.back(),
                                 transparency_buffer_mats_a.back(), 
                                 shadowdims(0),shadowdims(1),shadow_map_intensity);

      transparency_buffers.push_back(trans_buffer_temp);
    }
  }
  profile.mark("shadow_alpha_allocate");
  print_time(verbose, "Initialized alpha buffers" );
  
  
  ///
  //Parse mesh3d
  //
  List shapes = as<List>(mesh["shapes"]);
  int number_shapes = shapes.size();
  
  //Start by generating a shader for every material
  std::vector<material_info> mat_info;
  std::vector<std::unique_ptr<IShader>> shader_owners;
  std::vector<IShader*> shaders;

  // Count total faces across all shapes so we can size varying buffers correctly
  int total_faces = 0;
  for(int i = 0; i < number_shapes; i++) {
    List single_shape = as<List>(shapes(i));
    IntegerMatrix shape_inds = as<IntegerMatrix>(single_shape["indices"]);
    if(shape_inds.nrow() > std::numeric_limits<int>::max() - total_faces)
      throw std::overflow_error("Too many raster triangles");
    total_faces += shape_inds.nrow();
  }
  bool tangent_attributes=reference_buffers, vertex_intensity=reference_buffers;
  for(int type:typevals) { tangent_attributes |= type==5 || type==7; vertex_intensity |= type==1; }
  std::vector<vec3> vec_varying_intensity(vertex_intensity ? total_faces : 0, vec3(0.0));
  TriangleAttributes<vec3> vec_varying_uv(total_faces);
  TriangleAttributes<vec4> vec_varying_tri(total_faces);
  TriangleAttributes<vec3> vec_varying_pos(total_faces);
  TriangleAttributes<vec3> vec_varying_world_nrm(total_faces);
  TriangleAttributes<vec3> vec_varying_ndc_tri(tangent_attributes ? total_faces : 0);
  TriangleAttributes<vec3> vec_varying_nrm(tangent_attributes ? total_faces : 0);

  for(int i = 0; i < number_materials; i++) {
    List single_material = as<List>(materials(i));
    NumericVector ambient = as<NumericVector>(single_material["ambient"]);
    NumericVector diffuse = as<NumericVector>(single_material["diffuse"]);
    NumericVector specular = as<NumericVector>(single_material["specular"]);
    NumericVector transmittance = as<NumericVector>(single_material["transmittance"]);
    NumericVector emission = as<NumericVector>(single_material["emission"]);
    Float shininess = as<Float>(single_material["shininess"]);
    Float ior = as<Float>(single_material["ior"]);
    Float dissolve = as<Float>(single_material["dissolve"]);
    Float illum = as<Float>(single_material["illum"]);
    String ambient_texname = as<String>(single_material["ambient_texname"]);
    String diffuse_texname = as<String>(single_material["diffuse_texname"]);
    // String bump_texname = as<String>(single_material["bump_texname"]);
    String specular_texname = as<String>(single_material["specular_texname"]);
    String normal_texname = as<String>(single_material["normal_texname"]);
    String emissive_texname = as<String>(single_material["emissive_texname"]);
    Float diffuse_intensity = as<Float>(single_material["diffuse_intensity"]);
    // Float bump_intensity = as<Float>(single_material["bump_intensity"]);
    Float specular_intensity = as<Float>(single_material["specular_intensity"]);
    Float emission_intensity = as<Float>(single_material["emission_intensity"]);
    Float ambient_intensity = as<Float>(single_material["ambient_intensity"]);
    int cull_type = as<int>(single_material["culling"]);
    bool is_translucent = as<bool>(single_material["translucent"]);
    Float toon_levels = as<Float>(single_material["toon_levels"]);
	Float toon_outline_width = as<Float>(single_material["toon_outline_width"]);
	NumericVector toon_outline_color = as<NumericVector>(single_material["toon_outline_color"]);
    Float reflection_intensity = as<Float>(single_material["reflection_intensity"]);
    bool two_sided = as<bool>(single_material["two_sided"]);
    Float sigma = as<Float>(single_material["sigma"]);
    
    int type = typevals(i);

	if(type != 9 && type != 10) {
      toon_outline_width = 0.0f;
  	}
    //Change cull type to none if two sided
    cull_type = !two_sided ? cull_type : 3;
    
    bool has_texture_single          = has_texture(i);
    bool has_ambient_texture_single  = has_ambient_texture(i);
    // bool has_bump_texture_single     = has_bump_texture(i);
    bool has_normal_texture_single   = has_normal_texture(i);
    bool has_specular_texture_single = has_specular_texture(i);
    bool has_emissive_texture_single = has_emissive_texture(i);
    material_info temp = {
      vec3(ambient(0),ambient(1),ambient(2)),
      vec3(diffuse(0),diffuse(1),diffuse(2)),
      vec3(specular(0),specular(1),specular(2)),
      vec3(transmittance(0),transmittance(1),transmittance(2)), //Not used currently
      vec3(emission(0),emission(1),emission(2)),                //Not used currently
      shininess,
      ior,
      dissolve,
      illum,
      ambient_texname,
      diffuse_texname,
      specular_texname,
      normal_texname,
      emissive_texname,
      max_indices,
      (Float)emission_intensity,
      (Float)diffuse_intensity,
      (Float)specular_intensity,
      (Float)ambient_intensity,
      has_texture_single,
      has_ambient_texture_single,
      has_normal_texture_single,
      has_specular_texture_single,
      has_emissive_texture_single,
      cull_type,
      is_translucent,
      toon_levels,
	  toon_outline_width,
	  vec3(toon_outline_color(0),toon_outline_color(1),toon_outline_color(2)),
      reflection_intensity,
      sigma
    };
    temp.texture_cache = &texture_cache;
    mat_info.push_back(temp);

    IShader* shader;
    if(type == 1) {
      shader = new GouraudShader(Model, Projection, View, viewport,
                                 has_shadow_map, 
                                 shadow_map_bias,mat_info[i], point_lights,
                                 directional_lights, 
                                 shadowbuffers,
                                 transparency_buffers,
                                 vec_varying_intensity,
                                 vec_varying_uv,
                                 vec_varying_tri,
                                 vec_varying_pos,
                                 vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                 reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 2) {
      if(sigma <= 0) {
        shader = new DiffuseShader(Model, Projection, View, viewport,
                                   has_shadow_map,
                                   shadow_map_bias,mat_info[i], point_lights,
                                   directional_lights, 
                                   shadowbuffers,
                                   transparency_buffers,
                                   vec_varying_intensity,
                                   vec_varying_uv,
                                   vec_varying_tri,
                                   vec_varying_pos,
                                   vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                   reflection_maps[i], has_reflection_map(i), has_refraction(i),
                                   two_sided);
      } else {
        shader = new OrenNayerShader(Model, Projection, View, viewport,
                                   has_shadow_map,
                                   shadow_map_bias,mat_info[i], point_lights,
                                   directional_lights, 
                                   shadowbuffers,
                                   transparency_buffers,
                                   vec_varying_intensity,
                                   vec_varying_uv,
                                   vec_varying_tri,
                                   vec_varying_pos,
                                   vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                   reflection_maps[i], has_reflection_map(i), has_refraction(i),
                                   two_sided);
      }
    } else if (type == 3) {
      shader = new PhongShader(Model, Projection, View, viewport,
                               has_shadow_map,
                               shadow_map_bias,mat_info[i], point_lights,
                               directional_lights, 
                               shadowbuffers,
                               transparency_buffers,
                               vec_varying_intensity,
                               vec_varying_uv,
                               vec_varying_tri,
                               vec_varying_pos,
                               vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                               reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 4) {
      shader = new DiffuseNormalShader(Model, Projection, View, viewport,
                                       has_shadow_map,
                                       shadow_map_bias,mat_info[i], point_lights,
                                       directional_lights, 
                                       shadowbuffers,
                                       transparency_buffers,
                                       vec_varying_intensity,
                                       vec_varying_uv,
                                       vec_varying_tri,
                                       vec_varying_pos,
                                       vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                       reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 5) {
      shader = new DiffuseShaderTangent(Model, Projection, View, viewport,
                                        has_shadow_map,
                                        shadow_map_bias,mat_info[i], point_lights,
                                        directional_lights, 
                                        shadowbuffers,
                                        transparency_buffers,
                                        vec_varying_intensity,
                                        vec_varying_uv,
                                        vec_varying_tri,
                                        vec_varying_pos,
                                        vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                        reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 6) {
      shader = new PhongNormalShader(Model, Projection, View, viewport,
                                     has_shadow_map,
                                     shadow_map_bias,mat_info[i], point_lights,
                                     directional_lights,
                                     shadowbuffers,
                                     transparency_buffers,
                                     vec_varying_intensity,
                                     vec_varying_uv,
                                     vec_varying_tri,
                                     vec_varying_pos,
                                     vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                     reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 7) {
      shader = new PhongShaderTangent(Model, Projection, View, viewport,
                                      has_shadow_map,
                                      shadow_map_bias,mat_info[i], point_lights,
                                      directional_lights, 
                                      shadowbuffers,
                                      transparency_buffers,
                                      vec_varying_intensity,
                                      vec_varying_uv,
                                      vec_varying_tri,
                                      vec_varying_pos,
                                      vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                      reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 8) {
      shader = new ColorShader(Model, Projection, View, viewport,mat_info[i],
                               vec_varying_intensity,
                               vec_varying_uv,
                               vec_varying_tri,
                               vec_varying_pos,
                               vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                               reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 9) {
      shader = new ToonShader(Model, Projection, View, viewport,
                              has_shadow_map,
                              shadow_map_bias,mat_info[i], point_lights,
                              directional_lights, 
                              shadowbuffers,
                              transparency_buffers,
                              vec_varying_intensity,
                              vec_varying_uv,
                              vec_varying_tri,
                              vec_varying_pos,
                              vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                              reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else if (type == 10) {
      shader = new ToonShaderPhong(Model, Projection, View, viewport,
                              has_shadow_map,
                              shadow_map_bias,mat_info[i], point_lights,
                              directional_lights, 
                              shadowbuffers,
                              transparency_buffers,
                              vec_varying_intensity,
                              vec_varying_uv,
                              vec_varying_tri,
                              vec_varying_pos,
                              vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                              reflection_maps[i], has_reflection_map(i), has_refraction(i));
    } else {
      throw std::runtime_error("shader not recognized");
    }
    own_shader(shader_owners, shaders, shader);
  }

  reflection_map_info reflection_map_default {
    nullptr,
    1,
    1,
    1
  };
  
  //Initialize default material
  Rcpp::String fill("");
  material_info default_mat = {
    vec3(0.0,0.0,0.0),
    color,
    vec3(1.0),
    vec3(0.0),
    vec3(0.0),
    (Float)10.0,
    1.0,
    1.0,
    1.0,
    fill,fill,fill,fill,fill,
    max_indices,              //Maybe an issue?
    (Float)1.0,
    (Float)1.0,
    (Float)1.0,
    (Float)1.0,
    false,
    false,
    false,
    false,
    false,
    1,
    false,
    5,
	0.0,         // toon_outline_width
    vec3(0.0),   // toon_outline_color
    0.0,
    0
  };
  
  default_mat.texture_cache = &texture_cache;
  mat_info.push_back(default_mat);
  
  //Add default shader to vector
  if(typevals(0) == 1) {
    own_shader(shader_owners, shaders, new GouraudShader(Model, Projection, View, viewport,
                               has_shadow_map,
                               shadow_map_bias,mat_info.back(), point_lights,
                               directional_lights, 
                               shadowbuffers,
                               transparency_buffers,
                               vec_varying_intensity,
                               vec_varying_uv,
                               vec_varying_tri,
                               vec_varying_pos,
                               vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                               reflection_map_default, false, false));
  } else if (typevals(0) == 2 || typevals(0) == 4 || typevals(0) == 5) {
    own_shader(shader_owners, shaders, new DiffuseShader(Model, Projection, View, viewport,
                               has_shadow_map,
                               shadow_map_bias,mat_info.back(), point_lights,
                               directional_lights, 
                               shadowbuffers,
                               transparency_buffers,
                               vec_varying_intensity,
                               vec_varying_uv,
                               vec_varying_tri,
                               vec_varying_pos,
                               vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                               reflection_map_default, false, false, false));
  } else if (typevals(0) == 3 || typevals(0) == 6 || typevals(0) == 7) {
    own_shader(shader_owners, shaders, new PhongShader(Model, Projection, View, viewport,
                             has_shadow_map,
                             shadow_map_bias,mat_info.back(), point_lights,
                             directional_lights, 
                             shadowbuffers,
                             transparency_buffers,
                             vec_varying_intensity,
                             vec_varying_uv,
                             vec_varying_tri,
                             vec_varying_pos,
                             vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                             reflection_map_default, false, false));
  } else if (typevals(0) == 8) {
    own_shader(shader_owners, shaders, new ColorShader(Model, Projection, View, viewport,mat_info.back(),
                                      vec_varying_intensity,
                                      vec_varying_uv,
                                      vec_varying_tri,
                                      vec_varying_pos,
                                      vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                      reflection_map_default, false, false));
  } else if (typevals(0) == 9) {
    own_shader(shader_owners, shaders, new ToonShader(Model, Projection, View, viewport,
                                     has_shadow_map,
                                     shadow_map_bias,mat_info.back(), point_lights,
                                     directional_lights, 
                                     shadowbuffers,
                                     transparency_buffers,
                                     vec_varying_intensity,
                                     vec_varying_uv,
                                     vec_varying_tri,
                                     vec_varying_pos,
                                     vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                     reflection_map_default, false, false));
  } else if (typevals(0) == 10) {
    own_shader(shader_owners, shaders, new ToonShaderPhong(Model, Projection, View, viewport,
                                 has_shadow_map,
                                 shadow_map_bias,mat_info.back(), point_lights,
                                 directional_lights, 
                                 shadowbuffers,
                                 transparency_buffers,
                                 vec_varying_intensity,
                                 vec_varying_uv,
                                 vec_varying_tri,
                                 vec_varying_pos,
                                 vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                 reflection_map_default, false, false));
  } 
  profile.mark("shader_setup_asset_decode");
  print_time(verbose, "Initialized shaders" );
  
  
  //Initialize Model vectors
  std::vector<ModelInfo> models;

  //Fill vectors for each shape in the model
  //order: [model_num][triangle vertex][face]
  NumericMatrix mesh_verts = as<NumericMatrix>(mesh["vertices"]);
  NumericMatrix mesh_texcoords = as<NumericMatrix>(mesh["texcoords"]);
  NumericMatrix mesh_normals = as<NumericMatrix>(mesh["normals"]);
  
  int running_face_offset = 0;
  for(int i = 0; i < number_shapes; i++) {
    List single_shape = as<List>(shapes(i));
    IntegerMatrix shape_inds = as<IntegerMatrix>(single_shape["indices"]);
    IntegerMatrix tex_inds = as<IntegerMatrix>(single_shape["tex_indices"]);
    IntegerMatrix norm_inds = as<IntegerMatrix>(single_shape["norm_indices"]);
    IntegerVector shape_materials = as<IntegerVector>(single_shape["material_ids"]);

    int n = shape_inds.nrow();
    
    //Create model object
    ModelInfo model(mesh_verts, mesh_texcoords, mesh_normals,
                    shape_inds, tex_inds, norm_inds, 
                    has_vertex_tex, has_vertex_normals,
                    shape_materials,
                    has_normals_vec(i), has_tex_vec(i), tbn,
                    running_face_offset);
    models.push_back(model);
    running_face_offset += n;
  }
  // Cache only reused indexed positions. Keep the two existing multiplication
  // orders distinct: vp*(MVP*v) and (vp*MVP)*v are not numerically interchangeable.
  // Experimental: native wins were modest and one end-to-end workload regressed.
  // Keep this opt-in until a broader sweep justifies enabling it by default.
  const bool indexed = std::getenv("RAYVERTEX_INDEXED_TRANSFORMS") != nullptr && total_faces >= 1000 &&
    static_cast<std::size_t>(mesh_verts.nrow()) < 2*static_cast<std::size_t>(total_faces) &&
    std::getenv("RAYVERTEX_REFERENCE_TRANSFORMS") == nullptr;
  IndexedTransforms indexed_transforms;
  bool need_raw_clip = false, need_viewport_clip = false;
  for(const auto* shader : shaders) {
    need_raw_clip |= shader->uses_raw_clip();
    need_viewport_clip |= shader->uses_viewport_clip();
  }
  auto prepare_indexed = [&](const Mat& mvp, const Mat& viewport_matrix,
                             bool raw_clip, bool viewport_clip, bool view_positions) {
    if(!indexed) return;
    const std::size_t count = mesh_verts.nrow();
    if(!raw_clip) std::vector<vec4>().swap(indexed_transforms.clip);
    if(!viewport_clip) std::vector<vec4>().swap(indexed_transforms.viewport_clip);
    indexed_transforms.clip.resize(raw_clip ? count : 0);
    indexed_transforms.viewport_clip.resize(viewport_clip ? count : 0);
    indexed_transforms.view.resize(view_positions ? count : 0);
    const Mat combined = viewport_matrix * mvp;
    const Mat view = View * Model;
    for(std::size_t i = 0; i < count; ++i) {
      vec4 position(mesh_verts(i,0), mesh_verts(i,1), mesh_verts(i,2), 1.0);
      if(raw_clip) indexed_transforms.clip[i] = mvp * position;
      if(viewport_clip) indexed_transforms.viewport_clip[i] = combined * position;
      if(view_positions) indexed_transforms.view[i] = vec3(view * position);
    }
    for(auto& model : models) model.transforms = &indexed_transforms;
  };
  profile.mark("model_setup");
  print_time(verbose, "Initialized 3D models" );
  
  
  //For alpha transparency
  FragmentArena alpha_depths(nx, ny, block_size, fragment_mask);
  
  //For per-light transparent colors
  std::vector<FragmentArena> alpha_depths_trans;
  alpha_depths_trans.reserve(shadowbuffers.size());
  for (std::size_t i=0; i<shadowbuffers.size(); ++i)
    alpha_depths_trans.emplace_back(shadowdims(0), shadowdims(1), block_size, 0);

  TriangleBins blocks(nx,ny,block_size);
  blocks.triangles.reserve(total_faces);

  std::vector<std::vector<IShader*> > depthshaders(has_shadow_map ? directional_lights.size() : 0);
  for(unsigned int j = 0; j < depthshaders.size(); j++) {
    for(int i = 0; i < number_materials+1; i++ ) {
      own_shader(shader_owners, depthshaders[j], new DepthShader(Model, directional_lights[j].lightProjection,
                                                directional_lights[j].lightView, viewport_depth,
                                                mat_info[i],
                                                max_indices,
                                                vec_varying_uv,
                                                vec_varying_tri));
    }
  }

  #ifdef HAVE_THREADS
  RcppThread::ThreadPool pool(numbercores > 1 ? numbercores : 0);
  const int workers = numbercores;
  #else
  DummyThreadPool pool;
  const int workers = 1;
  #endif
  std::size_t main_tasks = 0, shadow_tasks = 0;
  std::vector<RasterCounters> main_counters(profile.enabled() ? blocks.size() : 0);
  profile.mark("bin_and_shadow_shader_allocate");
  if(has_shadow_map) {
    for(unsigned int sb = 0; sb < shadowbuffers.size(); sb++) {
      TriangleBins blocks_depth(nx_d,ny_d,block_size);
      blocks_depth.triangles.reserve(total_faces);
      prepare_indexed(directional_lights[sb].lightProjection * directional_lights[sb].lightView * Model,
                      vp_shadow, false, true, false);
      
      for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
        ModelInfo &shp = models[model_num];
        for(int i = 0; i < shp.num_indices; i++) {
          int mat_num = shp.materials[i] >= 0 && shp.materials[i] < (int)shaders.size() ? 
            shp.materials[i] : shaders.size()-1;
          
          std::array<vec4,3> clip;
          for(int k=0;k<3;++k) clip[k]=depthshaders[sb][mat_num]->vertex(i,k,shp);
          blocks_depth.add_clipped(clip,shp.index_offset+i,mat_num,depthshaders[sb][mat_num]->get_culling(),true);
        }
      }
      profile.mark("shadow_" + std::to_string(sb) + "_transform_setup");
      blocks_depth.build();
      profile.mark("shadow_" + std::to_string(sb) + "_bin_build");
      rayimage& shadowbuff = shadowbuffers[sb];
      std::vector<IShader*>& depth_shader_single = depthshaders[sb];
      FragmentArena& alpha_depth_single = alpha_depths_trans[sb];
      //Calculate shadow buffer
      auto task = [&](unsigned int i) {
        fill_tri_blocks(blocks_depth,i,depth_shader_single,zbuffer_depth,shadowbuff,
                        normalbuffer,positionbuffer,uvbuffer,true,alpha_depth_single,nullptr);
      };
      shadow_tasks += dispatch_raster_blocks(pool, blocks_depth, task, workers,
                                              batch_size, reference_scheduler);
      profile.mark("shadow_" + std::to_string(sb) + "_coverage_shading");
      // Resolve the exact depth-keyed winners, retaining opaque equality.
      auto resolve_shadow=[&](int i, int j, Float z, const alpha_info& fragment) {
        if(z <= zbuffer_depth(i,j)) {
          vec4 temp_col = fragment.color;
          vec4 old_color = transparency_buffers[sb].get_color_a(i,j);
          Float d = (1 - old_color.w) * (1 - temp_col.w);
          old_color *= temp_col;
          old_color.w = (1-d);
          transparency_buffers[sb].set_color(i,j,old_color);
        }
      };
      dispatch_fragment_tiles(pool,workers,alpha_depth_single,resolve_shadow);
      if(profile.enabled())
        profile.count("shadow_"+std::to_string(sb)+"_fragment_capacity_bytes",alpha_depth_single.capacity_bytes());
      transparency_buffers[sb].has_transparent_samples=alpha_depth_single.touched_samples()!=0;
      profile.count("shadow_"+std::to_string(sb)+"_has_transparent_samples",transparency_buffers[sb].has_transparent_samples);
      alpha_depth_single.release();
      profile.mark("shadow_" + std::to_string(sb) + "_transparency_resolve");
      std::fill(zbuffer_depth.begin(), zbuffer_depth.end(), std::numeric_limits<Float>::infinity() ) ;
      profile.mark("shadow_" + std::to_string(sb) + "_clear");
    }
  }
  profile.mark("shadow_finalize");
  print_time(verbose, "Calculated depth buffer(s)" );
  
  
  //Calculate Image
  prepare_indexed(Projection * View * Model, vp, need_raw_clip, need_viewport_clip, true);
  profile.mark("indexed_main_transforms");
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<Float>::infinity() ) ;

  for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(int i = 0; i < shp.num_indices; i++) {

      int mat_num = shp.materials[i] >= 0 && shp.materials[i] < (int)shaders.size() ?
        shp.materials[i] : shaders.size()-1;
      std::array<vec4,3> clip;
      for(int k=0;k<3;++k) {
        clip[k]=shaders[mat_num]->vertex(i,k,shp);
      }
      blocks.add_clipped(clip,shp.index_offset+i,mat_num,shaders[mat_num]->get_culling(),false);
    }
  }
  profile.mark("main_transform_setup");
  blocks.build();
  profile.mark("main_bin_build");
  auto task = [&](unsigned int i) {
    fill_tri_blocks(blocks,i,shaders,zbuffer,image,normalbuffer,positionbuffer,uvbuffer,
                    false,alpha_depths,requirements.outlines ? &material_id_buffer : nullptr,
                    main_counters.empty() ? nullptr : &main_counters[i],visibility);
  };

  main_tasks = dispatch_raster_blocks(pool, blocks, task, workers, batch_size,
                                      reference_scheduler);
  profile.mark("main_coverage_depth_shading");
  print_time(verbose, "Executed pixel shaders" );
  

  //Ambient occlusion
  if(calc_ambient) {
    constexpr unsigned int kernelSize=64;
    vec3 kernel[kernelSize];
    for (unsigned int i = 0; i < kernelSize; ++i) {
      kernel[i] = normalize(vec3(
        spacefillr::sobol_owen_single(i,0,0) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,1,0) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,2,0)));
      Float scale = Float(i) / Float(kernelSize);
      scale = lerp((Float)0.1, (Float)1.0, scale * scale);
      kernel[i] *= scale;
    }

    constexpr unsigned int noiseSize = ssao_noise_dimension * ssao_noise_dimension;
    vec3 noise[noiseSize];
    for (unsigned int i = 0; i < noiseSize; ++i) {
      noise[i] = normalize(vec3(
        spacefillr::sobol_owen_single(i,0,1) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,1,1) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,2,1)));
    }
    ScreenMatrixView nxbuffer_view(nxbuffer);
    ScreenMatrixView nybuffer_view(nybuffer);
    ScreenMatrixView nzbuffer_view(nzbuffer);
    ScreenMatrixView xxbuffer_view(xxbuffer);
    ScreenMatrixView yybuffer_view(yybuffer);
    ScreenMatrixView zzbuffer_view(zzbuffer);
    ScreenMatrixView abuffer_view(abuffer);
    dispatch_screen_rows(pool, workers, nx, [&](int x) {
      for (int y = 0; y < ny; y++) {
        if (nxbuffer_view(x,y) == 0 && nybuffer_view(x,y) == 0 && nzbuffer_view(x,y) == 0) {
          continue;
        }
        vec3 origin(xxbuffer_view(x,y), yybuffer_view(x,y), zzbuffer_view(x,y));
        vec3 normal(nxbuffer_view(x,y), nybuffer_view(x,y), nzbuffer_view(x,y));
        normal = normalize(normal);
        normal *= dot(normal, vec3(0,0,1)) < 0 ? -1 : 1;
        vec3 rvec = noise[ssao_noise_index(x, y)];
        vec3 tangent = normalize(rvec - normal * dot(rvec, normal));
        vec3 bitangent = cross(normal, tangent);
        glm::mat3 tbn{tangent, bitangent, normal};
        Float occlusion = 0.0;
        for (unsigned int i = 0; i < kernelSize; ++i) {
          // get sample position:
          vec3 sample = tbn * kernel[i];
          sample = sample * (Float)ambient_radius + origin;
          // project sample position:
          vec4 offset = vec4(sample, 1.0);
          offset = vp * Projection * offset;
          offset /= offset.w;

          if((int)offset.x >= 0 && (int)offset.x < nx && (int)offset.y >= 0 && (int)offset.y < ny) {
            Float sampleDepth = zzbuffer_view((int)offset.x, (int)offset.y);
            // range check & accumulate:
            Float rangeCheck= std::fabs(origin.z - sampleDepth) < (Float)ambient_radius ? 1.0 : 0.0;
            occlusion += (sampleDepth >= sample.z ? 1.0 : 0.0) * rangeCheck;
          }
        }
        occlusion = 1.0 - (occlusion / (Float)kernelSize);
        abuffer_view(x,y) = occlusion;
      }
    });
    NumericMatrix abuffer_noblur = clone(abuffer);
    const double* ambient_source = abuffer_noblur.begin();
    double* ambient_output = abuffer.begin();
    dispatch_screen_rows(pool, workers, nx, [&](int x) {
      blur_ambient_column(ambient_source, ambient_output, nx, ny, x);
    });
    profile.mark("ssao");
    print_time(verbose, "Calculated AO" );
    
  }
  
  
  std::vector<vec3> ndc_line_verts_start;
  std::vector<vec3> ndc_line_verts_end;
  std::vector<vec3> line_verts_cols;
  
  //Lines go here (no light)
  Mat vpMVP = vp * Projection * View * Model;
  for(int i = 0; i < line_mat.nrow(); i++) {
    vec4 temp_line_vertex_start = vpMVP * vec4(line_mat(i,0),line_mat(i,1),line_mat(i,2),1.0f);
    temp_line_vertex_start /= temp_line_vertex_start.w;
    vec4 temp_line_vertex_end = vpMVP * vec4(line_mat(i,3),line_mat(i,4),line_mat(i,5),1.0f);
    temp_line_vertex_end /= temp_line_vertex_end.w;
    ndc_line_verts_start.push_back(temp_line_vertex_start);
    ndc_line_verts_end.push_back(temp_line_vertex_end);
    line_verts_cols.push_back(vec3(line_mat(i,6),line_mat(i,7),line_mat(i,8)));
  }
  
  if(line_mat.nrow() > 0) {
    if(aa_lines) {
      aa_line(ndc_line_verts_start, ndc_line_verts_end, line_verts_cols, zbuffer, alpha_depths, alpha_line, line_offset);
    } else {
      noaa_line(ndc_line_verts_start, ndc_line_verts_end, line_verts_cols, zbuffer, alpha_depths, alpha_line, line_offset);
    }
  }

  profile.mark("lines");
  auto resolve_main=[&](int i, int j, Float z, const alpha_info& fragment) {
    if(z <= zbuffer(i,j)) {
      zbuffer(i,j) = z;
      vec4 temp_col = fragment.color;
      vec3 old_color = image.get_color(i,j);
      vec3 new_color = vec3(temp_col)*temp_col.w + vec3(old_color)*(1-temp_col.w);
      image.set_color(i,j,new_color);
      normalbuffer.set_color(i,j,fragment.normal);
      positionbuffer.set_color(i,j,fragment.position);
      uvbuffer.set_color(i,j,fragment.uv);
    }
  };
  dispatch_fragment_tiles(pool,workers,alpha_depths,resolve_main);

  if(profile.enabled()) {
    profile.count("fragment_capacity_bytes", alpha_depths.capacity_bytes());
    profile.count("maximum_layers_per_sample", alpha_depths.max_layers());
    profile.count("transparent_touched_samples", alpha_depths.touched_samples());
  }
  alpha_depths.release();
  profile.mark("transparency_resolve");
  //Load/blur environment image
  if(has_environment_map) {
    const reflection_map_info background_map=environment_variant(background_sharpness,
                                                                 background_sharpness!=1.0);
    Float theta = fov * M_PI/180;
    Float half_height = tan(theta/2);
    Float half_width = Float(nx)/Float(ny) * half_height;
    vec3 origin = eye;
    vec3 w = glm::normalize(eye - center);
    vec3 u = glm::normalize(glm::cross(cam_up, w));
    vec3 v = glm::cross(w, u);
    vec3 lower_left_corner = origin - half_width *  u - half_height * v - w;
    vec3 horizontal = 2.0f * half_width * u;
    vec3 vertical = 2.0f * half_height * v;
    dispatch_screen_rows(pool,workers,nx,[&](int i) {
      for(int j = 0; j < ny; j++) {
        if(std::isinf(zbuffer(i,j))) {
          Float s = (Float(i)) / Float(nx);
          Float t = (Float(j)) / Float(ny);
          vec2 uv;
          vec3 dir = glm::normalize(lower_left_corner + s * horizontal + t * vertical - origin);
          get_sphere_uv(dir,uv);
          uv.x = 1 - uv.x;
          uv.x += 0.25;
          vec3 ref_color = trivalue(uv.x,uv.y,background_map);
          image.set_color(i,j,ref_color);
        }
      }
    });
    profile.mark("environment_fill");
    print_time(verbose, "Blurred environment map" );
  }
  
  // Raster depth remains [0,1], with infinity for uncovered samples.
  // Exported depth retains the legacy [-1,1], background=1 convention.
  NumericMatrix linear_depth(0,0);
  if(need_linear_depth) {
  linear_depth = clone(zbuffer);
  for(unsigned int i = 0; i < linear_depth.nrow(); i++) {
    for(unsigned int j= 0; j < linear_depth.ncol(); j++) {
      if(std::isinf(linear_depth(i,j))) {
        linear_depth(i,j) = 1;
      }
      linear_depth(i,j) = 2*linear_depth(i,j) - 1;
    }
  }
  if(fov!=0.0)
    linear_depth = 2*near_clip*far_clip/(far_clip + near_clip - linear_depth * (far_clip-near_clip));
  else
    linear_depth = near_clip+(linear_depth+1.0)*0.5*(far_clip-near_clip);
  }
  profile.mark("depth_conversion");
  print_time(verbose, "Calculated linear depth" );

  if (requirements.outlines) {
    // Build color buffer and outline g-buffer for JFA
    std::size_t pix_count =
        static_cast<std::size_t>(nx) * static_cast<std::size_t>(ny);
    std::vector<vec3> toon_color_buffer(pix_count);
    std::vector<OutlineGBufferPixel> outline_gbuffer(pix_count);

    dispatch_screen_rows(pool,workers,nx,[&](int x) {
      for (int y = 0; y < ny; ++y) {
        std::size_t idx =
            static_cast<std::size_t>(y) * static_cast<std::size_t>(nx) + x;

        toon_color_buffer[idx] = image.get_color(x, y);
        OutlineGBufferPixel &px = outline_gbuffer[idx];

        // --- Decide if this pixel has geometry ---
        bool z_is_inf = std::isinf(zbuffer(x, y));

        int raw_mat_id = -1;
        if (material_id_buffer.nrow() == nx && material_id_buffer.ncol() == ny) {
          raw_mat_id = material_id_buffer(x, y);
        }
        bool has_material =
            (raw_mat_id >= 0 && raw_mat_id < (int)mat_info.size());

        bool has_normal = (nxbuffer(x, y) != 0.0 || nybuffer(x, y) != 0.0 ||
                           nzbuffer(x, y) != 0.0);

        bool has_geom = !z_is_inf && has_material && has_normal;

        if (!has_geom) {
          // Background: no geometry
          px.normal_view = vec3(0.0);
          px.depth_view = std::numeric_limits<Float>::infinity();
          px.material_id = 0u;
          px.outline_width = 0.0;
          px.outline_color = vec3(0.0);
          px.has_outline = false;
          continue;
        }

        int mat_id = raw_mat_id;
        if (mat_id < 0 || mat_id >= (int)mat_info.size()) {
          mat_id = 0;
        }

        px.normal_view = vec3(nxbuffer(x, y), nybuffer(x, y), nzbuffer(x, y));
        px.depth_view = linear_depth(x, y);
        px.material_id = static_cast<std::uint32_t>(mat_id);

        const material_info &m = mat_info[mat_id];

        Float outline_width = m.toon_outline_width;
        vec3 outline_color = m.toon_outline_color;

        px.outline_width = outline_width;
        px.outline_color = outline_color;
        px.has_outline = (outline_width > (Float)0.0);
      }
    });

    Float camera_fov_y = fov != 0.0 ? glm::radians((Float)fov) : (Float)0.0;
    Float ortho_view_height = 0.0;
    if (fov == 0.0 && ortho_dims.size() > 1) {
      ortho_view_height = static_cast<Float>(ortho_dims(1));
    }

    apply_toon_outlines_jfa(pool, workers, toon_color_buffer, outline_gbuffer, nx, ny,
                            camera_fov_y, ortho_view_height, zbuffer, linear_depth);

    // Copy the modified colors back into the rayimage
    dispatch_screen_rows(pool,workers,nx,[&](int x) {
      for (int y = 0; y < ny; ++y) {
        std::size_t idx =
            static_cast<std::size_t>(y) * static_cast<std::size_t>(nx) + x;
        image.set_color(x, y, toon_color_buffer[idx]);
      }
    });

    profile.mark("outlines");
    print_time(verbose, "Applied toon outline JFA pass" );
  }

  profile.mark("remaining_output_setup");
  profile.count("outline_scratch_payload_bytes", requirements.outlines ?
    checked_samples(nx, ny) * (sizeof(vec3) + sizeof(OutlineGBufferPixel) + 2*sizeof(JFASeed)) : 0);
  profile.count("shadow_matrix_payload_bytes", has_shadow_map ?
    checked_samples(nx_d, ny_d) * sizeof(double) * (1 + 5*directional_lights.size()) : 0);
  profile.count("varying_payload_bytes", static_cast<std::size_t>(total_faces) *
    ((vertex_intensity ? sizeof(vec3) : 0) + (3+2*tangent_attributes)*sizeof(std::array<vec3, 3>) + sizeof(std::array<vec4, 3>)));
  profile.count("auxiliary_matrix_payload_bytes", checked_samples(nx,ny)*sizeof(double)*
    (3*need_normals+3*need_positions+3*need_uv+need_linear_depth+need_ambient));
  profile.count("environment_variant_requests",environment_variant_requests);
  profile.count("environment_resize_variants",reflection_variants.size());
  profile.count("environment_variant_bytes",environment_variant_bytes);
  profile.count("texture_decodes", texture_cache.decodes + (has_environment_map ? 1 : 0));
  profile.count("texture_payload_bytes", texture_cache.payload_bytes);
  profile.count("prepared_texture_hits",texture_cache.hits);
  profile.count("prepared_texture_payload_bytes",prepared_assets ? prepared_assets->payload_bytes : 0);
  profile.count("indexed_positions", indexed ? mesh_verts.nrow() : 0);
  if (profile.enabled()) {
    std::size_t clip_evaluations = indexed ?
      static_cast<std::size_t>(mesh_verts.nrow()) * (need_raw_clip + need_viewport_clip) : 0;
    if (!indexed) {
      // Some shaders evaluate both clip forms for every corner. Count the
      // actual face's shader, including fallback materials, outside hot loops.
      for (auto& model : models) {
        for (int face = 0; face < model.num_indices; ++face) {
          int material = model.materials[face];
          if (material < 0 || material >= static_cast<int>(shaders.size()))
            material = static_cast<int>(shaders.size()) - 1;
          IShader* shader = shaders[material];
          clip_evaluations += 3 * (shader->uses_raw_clip() + shader->uses_viewport_clip());
        }
      }
    }
    profile.count("main_clip_transform_evaluations", clip_evaluations);
  }
  profile.count("indexed_transform_payload_bytes", indexed_transforms.clip.capacity()*sizeof(vec4) +
    indexed_transforms.viewport_clip.capacity()*sizeof(vec4) + indexed_transforms.view.capacity()*sizeof(vec3));
  profile.count("input_triangles", total_faces);
  profile.count("models", models.size());
  profile.count("materials", mat_info.size());
  profile.count("main_tasks", main_tasks);
  profile.count("shadow_tasks", shadow_tasks);
  if(profile.enabled()) {
    std::size_t active=0;
    for(std::size_t i=0;i<blocks.size();++i) active+=blocks.active(i);
    profile.count("main_active_blocks", active);
    profile.count("main_bin_references", blocks.references());
    profile.count("main_setup_count", blocks.attempted);
    profile.count("main_post_clip_triangles", blocks.attempted);
    profile.count("main_clipped_triangles", blocks.clip_weights.size());
    profile.count("main_culled_primitives", blocks.culled);
    profile.count("main_setup_bin_capacity_bytes", blocks.capacity_bytes());
  }
  RasterCounters totals;
  for(const auto& c : main_counters) {
    totals.candidates += c.candidates; totals.covered += c.covered;
    totals.early_z += c.early_z; totals.shaded += c.shaded;
    totals.transparent += c.transparent;
    totals.visibility_tiles+=c.visibility_tiles;
    totals.visibility_fallbacks+=c.visibility_fallbacks;
    totals.visibility_coverage_ms+=c.visibility_coverage_ms;
    totals.visibility_shading_ms+=c.visibility_shading_ms;
  }
  profile.count("coverage_candidates", totals.candidates);
  profile.count("covered_samples", totals.covered);
  profile.count("early_z_failures", totals.early_z);
  profile.count("shader_calls", totals.shaded);
  profile.count("transparent_fragments", totals.transparent);
  profile.count("visibility_tiles",totals.visibility_tiles);
  profile.count("visibility_fallbacks",totals.visibility_fallbacks);
  profile.count("visibility_coverage_worker_ms",totals.visibility_coverage_ms);
  profile.count("visibility_shading_worker_ms",totals.visibility_shading_ms);
  NumericMatrix presentation_depth = clone(zbuffer);
  for(auto& value : presentation_depth) value = std::isinf(value) ? 1.0 : 2*value - 1;
  List output = List::create(_["r"] = r, _["g"] = g, _["b"] = b, _["a"] = a,
                      _["amb"] = abuffer, _["depth"] = presentation_depth, _["linear_depth"] = linear_depth,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer,
                      _["uvx"] = uvxbuffer, _["uvy"] = uvybuffer, _["uvz"] = uvzbuffer);
  profile.mark("native_output_assembly");
  profile.finish();
  return output;
}

#endif
