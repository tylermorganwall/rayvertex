#define STB_IMAGE_IMPLEMENTATION 
#define STB_IMAGE_RESIZE_IMPLEMENTATION

#ifndef RAYRASTERH
#define RAYRASTERH

#include "Rcpp.h"

#define FLOAT_AS_DOUBLE
// #ifndef FLOAT_AS_DOUBLE
// typedef float Float;
// #else 
// typedef double Float;
// #endif

#include <functional>
#include <algorithm>
#include <utility>
#include "stb_image.h"
#include "stb_image_resize.h"
#include <memory>
#include "glm.hpp"
#include "gtc/matrix_transform.hpp"
#include "defines.h"
#include "filltri.h"

#include "shaders.h"
#include "rayimage.h"
#include "model.h"
#include "single_sample.h"
// [[Rcpp::depends(RcppThread)]]
#include "RcppThread.h"

#include "material.h"

#include "light.h"
#include "line.h"

// typedef glm::dvec4 vec4;
// typedef glm::dvec3 vec3;
// typedef glm::dvec2 vec2;
// typedef glm::dmat4x4 Mat;

// static void print_vec(vec3 m) {
//   RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << "\n";
// }
// 
// static void print_vec(glm::dvec4 m) {
//   RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << "\n";
// }

using namespace Rcpp;

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

// void print_mat(Mat m) {
//   m = glm::transpose(m);
//   Rcpp::Rcout.precision(5);
//   Rcpp::Rcout << std::fixed << m[0][0] << " " << m[0][1] << " " << m[0][2] << " " << m[0][3] << "\n";
//   Rcpp::Rcout << std::fixed << m[1][0] << " " << m[1][1] << " " << m[1][2] << " " << m[1][3] << "\n";
//   Rcpp::Rcout << std::fixed << m[2][0] << " " << m[2][1] << " " << m[2][2] << " " << m[2][3] << "\n";
//   Rcpp::Rcout << std::fixed << m[3][0] << " " << m[3][1] << " " << m[3][2] << " " << m[3][3] << "\n";
// }


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
               LogicalVector has_refraction, bool environment_map_hdr) {
  List materials = as<List>(mesh["materials"]);
  int number_materials = materials.size();
  
  //Turn off gamma correction or get seams in textures/normal maps
  stbi_ldr_to_hdr_gamma(1.0f);
  
  //Resize reflection map for different roughness materials
  float* reflection_map_data = nullptr;
  int nx_r = 0, ny_r = 0, nn_r = 0;
  std::vector<reflection_map_info> reflection_maps;
  reflection_map_info main_reflection_map;
  std::vector<float* > reflection_data;
  bool loaded_reflection_map = false;
  
  for(unsigned int i = 0; i < has_reflection_map.size(); i++) {
    if(has_reflection_map(i) || has_refraction(i)) {
      List single_material = as<List>(materials(i));
      double reflection_sharpness = as<double>(single_material["reflection_sharpness"]);
      if(!loaded_reflection_map) {
        reflection_map_data = stbi_loadf(reflection_map_file.get_cstring(), &nx_r, &ny_r, &nn_r, 0);
        if(nx_r == 0 || ny_r == 0 || nn_r == 0) {
          throw std::runtime_error("Reflection map loading failed");
        }
        if(environment_map_hdr) {
          float gamma_exp = 1.0/2.2f;
          for(int j = 0; j < nx_r * ny_r * nn_r; j++) {
            reflection_map_data[j] = powf(reflection_map_data[j],gamma_exp);
          }
        }
        loaded_reflection_map = true;
        main_reflection_map.reflection = reflection_map_data;
        main_reflection_map.nx = nx_r;
        main_reflection_map.ny = ny_r;
        main_reflection_map.nn = nn_r;
      }
      
      int nx_r_resize = (double)nx_r * reflection_sharpness;
      int ny_r_resize = (double)ny_r * reflection_sharpness;
      float* reflection_map_data_new = new float[nx_r * ny_r * nn_r];
      if(reflection_sharpness < 1.0 && reflection_sharpness > 0.0) {
        float* reflection_map_data_temp = new float[nx_r_resize * ny_r_resize * nn_r];
        stbir_resize_float_generic(reflection_map_data, nx_r, ny_r, 0, 
                                   reflection_map_data_temp, nx_r_resize, ny_r_resize, 0,
                                   nn_r, 0, 0, STBIR_EDGE_WRAP, STBIR_FILTER_CUBICBSPLINE, STBIR_COLORSPACE_LINEAR, NULL);
        
        stbir_resize_float_generic(reflection_map_data_temp, nx_r_resize, ny_r_resize, 0, 
                                   reflection_map_data_new, nx_r, ny_r, 0,
                                   nn_r, 0, 0, STBIR_EDGE_WRAP, STBIR_FILTER_CUBICBSPLINE, STBIR_COLORSPACE_LINEAR, NULL);
        delete[] reflection_map_data_temp;
      } else {
        memcpy(reflection_map_data_new, main_reflection_map.reflection, sizeof(float) * nx_r * ny_r * nn_r);
      }
      reflection_data.push_back(reflection_map_data_new);
      reflection_map_info reflection_map {
        reflection_map_data_new,
        nx_r,
        ny_r,
        nn_r
      };
      reflection_maps.push_back(reflection_map);
    } else {
      reflection_map_info reflection_map {
        nullptr,
        nx_r,
        ny_r,
        nn_r
      };
      reflection_maps.push_back(reflection_map);
    }
  }
  
  //Convert R vectors to vec3
  vec3 eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));    //lookat
  vec3 cam_up = vec3(camera_up(0),camera_up(1),camera_up(2));
  vec3 color(model_color(0),model_color(1),model_color(2));

  //Account for colinear camera direction/cam_up vectors
  if(glm::length(glm::cross(eye-center,cam_up)) == 0) {
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
                                    (Float) scene_diag + dist_to_focus) :
    glm::ortho(-(Float)ortho_dims(0)/2, (Float)ortho_dims(0)/2, -(Float)ortho_dims(1)/2, (Float)ortho_dims(1)/2);
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
  
  //Create buffers
  rayimage image(r,g,b,nx,ny);
  
  //Depth buffer
  NumericMatrix zbuffer(nx,ny);
  NumericMatrix sbuffer(shadowdims(0),shadowdims(1));
  NumericMatrix zbuffer_depth(shadowdims(0),shadowdims(1));
  
  NumericMatrix abuffer(nx,ny);
  
  //Fill ambient occlusion buffer
  std::fill(abuffer.begin(), abuffer.end(), 1.0f ) ;
  
  
  //Position space buffer
  NumericMatrix xxbuffer(nx,ny);
  NumericMatrix yybuffer(nx,ny);
  NumericMatrix zzbuffer(nx,ny);
  
  //Normal space buffer
  NumericMatrix nxbuffer(nx,ny);
  NumericMatrix nybuffer(nx,ny);
  NumericMatrix nzbuffer(nx,ny);
  
  //UV buffer
  NumericMatrix uvxbuffer(nx,ny);
  NumericMatrix uvybuffer(nx,ny);
  NumericMatrix uvzbuffer(nx,ny);

  //Initialize rayimage buffers
  rayimage shadowbuffer(sbuffer, shadowdims(0), shadowdims(1),shadow_map_intensity);
  rayimage ambientbuffer(abuffer, nx, ny);
  rayimage positionbuffer(xxbuffer,yybuffer,zzbuffer,nx,ny);
  rayimage normalbuffer(nxbuffer,nybuffer,nzbuffer,nx,ny);
  rayimage uvbuffer(uvxbuffer,uvybuffer,uvzbuffer,nx,ny);
  
  //Initialize zbuffer
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<Float>::infinity() ) ;
  std::fill(zbuffer_depth.begin(), zbuffer_depth.end(), std::numeric_limits<Float>::infinity() ) ;
  
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
      shadowbuffer_mats.push_back(NumericMatrix(shadowdims(0),shadowdims(1)));
      std::fill(shadowbuffer_mats.back().begin(), shadowbuffer_mats.back().end(), 
                std::numeric_limits<double>::infinity() ) ;
      
      rayimage shadowbuffer_temp(shadowbuffer_mats.back(),shadowdims(0),shadowdims(1),shadow_map_intensity);
      
      shadowbuffers.push_back(shadowbuffer_temp);
      
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
    }
  }
  
  std::vector<rayimage > transparency_buffers;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_r;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_g;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_b;
  std::vector<Rcpp::NumericMatrix> transparency_buffer_mats_a;
  
  for(int i = 0; i < is_dir_light.length(); i++) {
    if(is_dir_light(i)) {
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
  
  ///
  //Parse mesh3d
  //
  
  //Start by generating a shader for every material
  std::vector<material_info> mat_info;
  std::vector<IShader*> shaders;

  
  std::vector<vec3> vec_varying_intensity;
  std::vector<std::vector<vec3> > vec_varying_uv;
  std::vector<std::vector<vec4> > vec_varying_tri;
  std::vector<std::vector<vec3> > vec_varying_pos;
  std::vector<std::vector<vec3> > vec_varying_world_nrm;
  std::vector<std::vector<vec3> > vec_varying_ndc_tri;
  std::vector<std::vector<vec3> > vec_varying_nrm;
  
  for(int i = 0; i < max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    std::vector<vec3> tempndc(3);
    std::vector<vec3> tempnrm2(3);

    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
    vec_varying_ndc_tri.push_back(tempndc);
    vec_varying_nrm.push_back(tempnrm2);
  }
  
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
    String specular_texname = as<String>(single_material["specular_texname"]);
    String normal_texname = as<String>(single_material["normal_texname"]);
    String emissive_texname = as<String>(single_material["emissive_texname"]);
    Float diffuse_intensity = as<Float>(single_material["diffuse_intensity"]);
    Float specular_intensity = as<Float>(single_material["specular_intensity"]);
    Float emission_intensity = as<Float>(single_material["emission_intensity"]);
    Float ambient_intensity = as<Float>(single_material["ambient_intensity"]);
    int cull_type = as<int>(single_material["culling"]);
    bool is_translucent = as<bool>(single_material["translucent"]);
    Float toon_levels = as<Float>(single_material["toon_levels"]);
    Float reflection_intensity = as<Float>(single_material["reflection_intensity"]);
    
    
    bool has_texture_single          = has_texture(i);
    bool has_ambient_texture_single  = has_ambient_texture(i);
    
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
      reflection_intensity
    };
    mat_info.push_back(temp);
    
    IShader* shader;
    int type = typevals(i);
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
                                 reflection_maps[i], has_reflection_map(i), has_refraction(i));
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
    shaders.push_back(shader);
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
    0.0
  };
  
  mat_info.push_back(default_mat);
  
  //Add default shader to vector
  if(typevals(0) == 1) {
    shaders.push_back(new GouraudShader(Model, Projection, View, viewport,
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
    shaders.push_back(new DiffuseShader(Model, Projection, View, viewport,
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
  } else if (typevals(0) == 3 || typevals(0) == 6 || typevals(0) == 7) {
    shaders.push_back(new PhongShader(Model, Projection, View, viewport,
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
    shaders.push_back(new ColorShader(Model, Projection, View, viewport,mat_info.back(),
                                      vec_varying_intensity,
                                      vec_varying_uv,
                                      vec_varying_tri,
                                      vec_varying_pos,
                                      vec_varying_world_nrm,vec_varying_ndc_tri,vec_varying_nrm,
                                      reflection_map_default, false, false));
  } else if (typevals(0) == 9) {
    shaders.push_back(new ToonShader(Model, Projection, View, viewport,
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
    shaders.push_back(new ToonShaderPhong(Model, Projection, View, viewport,
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
  
  
  //Initialize Model vectors
  List shapes = as<List>(mesh["shapes"]);
  int number_shapes = shapes.size();
  std::vector<ModelInfo> models;

  //Initialize vertex storage vectors
  std::vector<std::vector<std::vector<vec4>  > > ndc_verts;
  std::vector<std::vector<std::vector<Float> > > ndc_inv_w;
  std::vector<std::vector<std::vector<vec4>  > > ndc_verts_depth;
  std::vector<std::vector<std::vector<Float> > > ndc_inv_w_depth;
  
  //Fill vectors for each shape in the model
  //order: [model_num][triangle vertex][face]
  NumericMatrix mesh_verts = as<NumericMatrix>(mesh["vertices"]);
  NumericMatrix mesh_texcoords = as<NumericMatrix>(mesh["texcoords"]);
  NumericMatrix mesh_normals = as<NumericMatrix>(mesh["normals"]);
  
  
  
  for(int i = 0; i < number_shapes; i++) {
    List single_shape = as<List>(shapes(i));
    IntegerMatrix shape_inds = as<IntegerMatrix>(single_shape["indices"]);
    IntegerMatrix tex_inds = as<IntegerMatrix>(single_shape["tex_indices"]);
    IntegerMatrix norm_inds = as<IntegerMatrix>(single_shape["norm_indices"]);
    IntegerVector shape_materials = as<IntegerVector>(single_shape["material_ids"]);

    int n = shape_inds.nrow();
    
    ndc_verts.push_back(std::vector<std::vector<vec4>  >(3, std::vector<vec4>(n)));
    ndc_inv_w.push_back(std::vector<std::vector<Float> >(3, std::vector<Float>(n)));
    if(has_shadow_map) {
      ndc_verts_depth.push_back(std::vector<std::vector<vec4>  >(3, std::vector<vec4>(n)));
      ndc_inv_w_depth.push_back(std::vector<std::vector<Float> >(3, std::vector<Float>(n)));
    }
    
    //Create model object
    ModelInfo model(mesh_verts, mesh_texcoords, mesh_normals,
                    shape_inds, tex_inds, norm_inds, 
                    has_vertex_tex, has_vertex_normals,
                    shape_materials,
                    has_normals_vec(i), has_tex_vec(i), tbn);
    models.push_back(model);
  }
  
  //For alpha transparency
  std::vector<std::map<Float, alpha_info> > alpha_depths(nx*ny);
  
  //For per-light transparent colors
  std::vector<std::vector<std::map<Float, alpha_info> > > alpha_depths_trans;
  for(unsigned int i = 0; i < shadowbuffers.size(); i++) {
    std::vector<std::map<Float, alpha_info> > temp_adt(shadowdims(0) * shadowdims(1));
    alpha_depths_trans.push_back(temp_adt);
  }
  
  
  //Set up blocks
  int blocksize = block_size;
  
  //Inner-most index model, then block, then index
  std::vector<std::vector<std::vector<int> > > blocks;
  
  std::vector<vec2> min_block_bound;
  std::vector<vec2> max_block_bound;
  int nx_blocks = ceil((Float)nx/(Float)blocksize);
  int ny_blocks = ceil((Float)ny/(Float)blocksize);
  
  std::vector<std::vector<int> > single_model_blocks;

  //Generate block bounds
  for(int i = 0; i < nx; i += blocksize) {
    for(int j = 0; j < ny; j += blocksize) {
      min_block_bound.push_back(vec2(i,j));
      max_block_bound.push_back(vec2(std::min(i+blocksize,nx),std::min(j+blocksize,ny)));
    }
  }
  
  //Generate a group of index vectors for each model
  for(int i = 0; i < nx_blocks; i++) {
    for(int j = 0; j < ny_blocks; j++) {
      std::vector<std::vector<int> > temp;
      blocks.push_back(temp);
      //This is a vector of all the models for that block
      //blocks[j + ny_blocks * i]
      for(unsigned int k = 0; k < models.size(); k++) {
        std::vector<int> model_inds_temp;
        //This is a vector for per-model indices in that specific block
        //blocks[j + ny_blocks * i][model_num]
        blocks[j + ny_blocks * i].push_back(model_inds_temp);
      }
    }
  }
  
  //Light, Inner-most index model, then block, then index
  std::vector<std::vector<std::vector<int> > > blocks_depth;
  
  std::vector<vec2> min_block_bound_depth;
  std::vector<vec2> max_block_bound_depth;
  int nx_blocks_depth = ceil((Float)nx_d/(Float)blocksize);
  int ny_blocks_depth = ceil((Float)ny_d/(Float)blocksize);
  
  std::vector<std::vector<int> > single_model_blocks_depth;
  
  //Generate block bounds
  for(int i = 0; i < nx_d; i += blocksize) {
    for(int j = 0; j < ny_d; j += blocksize) {
      min_block_bound_depth.push_back(vec2(i,j));
      max_block_bound_depth.push_back(vec2(std::min(i+blocksize,nx_d),std::min(j+blocksize,ny_d)));
    }
  }
  
  //Generate a group of index vectors for each model
  for(int i = 0; i < nx_blocks_depth; i++) {
    for(int j = 0; j < ny_blocks_depth; j++) {
      std::vector<std::vector<int> > temp;
      blocks_depth.push_back(temp);
      //This is a vector of all the models for that block
      //blocks[j + ny_blocks * i]
      for(unsigned int k = 0; k < models.size(); k++) {
        std::vector<int> model_inds_temp;
        //This is a vector for per-model indices in that specific block
        //blocks[j + ny_blocks * i][model_num]
        blocks_depth[j + ny_blocks_depth * i].push_back(model_inds_temp);
      }
    }
  }
  
  std::vector<std::vector<IShader*> > depthshaders(directional_lights.size());
  for(unsigned int j = 0; j < directional_lights.size(); j++) {
    for(int i = 0; i < number_materials+1; i++ ) {
      depthshaders[j].push_back(new DepthShader(Model, directional_lights[j].lightProjection, 
                                                directional_lights[j].lightView, viewport_depth,
                                                mat_info[i],
                                                max_indices,
                                                vec_varying_uv,
                                                vec_varying_tri));
    }
  }

  if(has_shadow_map) {
    for(unsigned int sb = 0; sb < shadowbuffers.size(); sb++) {
      
      for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
        ModelInfo &shp = models[model_num];
        for(int i = 0; i < shp.num_indices; i++) {
          int mat_num = shp.materials[i] >= 0 && shp.materials[i] < (int)shaders.size() ? 
            shp.materials[i] : shaders.size()-1;
          
          ndc_verts_depth[model_num][0][i] = depthshaders[sb][mat_num]->vertex(i,0, shp);
          ndc_verts_depth[model_num][1][i] = depthshaders[sb][mat_num]->vertex(i,1, shp);
          ndc_verts_depth[model_num][2][i] = depthshaders[sb][mat_num]->vertex(i,2, shp);
          
          ndc_verts[model_num][0][i].w = ndc_verts[model_num][0][i].w < near_clip ? near_clip : ndc_verts[model_num][0][i].w;
          ndc_verts[model_num][1][i].w = ndc_verts[model_num][1][i].w < near_clip ? near_clip : ndc_verts[model_num][1][i].w;
          ndc_verts[model_num][2][i].w = ndc_verts[model_num][2][i].w < near_clip ? near_clip : ndc_verts[model_num][2][i].w;
    
          ndc_inv_w_depth[model_num][0][i] = 1.0f/ndc_verts_depth[model_num][0][i].w;
          ndc_inv_w_depth[model_num][1][i] = 1.0f/ndc_verts_depth[model_num][1][i].w;
          ndc_inv_w_depth[model_num][2][i] = 1.0f/ndc_verts_depth[model_num][2][i].w;
          
          vec3 v1 = ndc_verts_depth[model_num][0][i] * ndc_inv_w_depth[model_num][0][i];
          vec3 v2 = ndc_verts_depth[model_num][1][i] * ndc_inv_w_depth[model_num][1][i];
          vec3 v3 = ndc_verts_depth[model_num][2][i] * ndc_inv_w_depth[model_num][2][i];
          
          vec3 min_bounds = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                                 fmin(v1.y,fmin(v2.y,v3.y)),
                                 fmin(v1.z,fmin(v2.z,v3.z)));
          
          vec3 max_bounds = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                                 fmax(v1.y,fmax(v2.y,v3.y)),
                                 fmax(v1.z,fmax(v2.z,v3.z)));
          
          int min_x_block = std::fmax(floor(min_bounds.x / (Float)blocksize), 0);
          int min_y_block = std::fmax(floor(min_bounds.y / (Float)blocksize), 0);
          int max_x_block = std::fmin(ceil(max_bounds.x  / (Float)blocksize), nx_blocks_depth);
          int max_y_block = std::fmin(ceil(max_bounds.y  / (Float)blocksize), ny_blocks_depth);
          if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks_depth && min_y_block < ny_blocks_depth) {
            for(int j = min_x_block; j < max_x_block; j++) {
              for(int k = min_y_block; k < max_y_block; k++) {
                blocks_depth[k + ny_blocks_depth * j][model_num].push_back(i); 
              }
            }
          }
        }
      }
      
      rayimage& shadowbuff = shadowbuffers[sb];
      std::vector<IShader*>& depth_shader_single = depthshaders[sb];
      std::vector<std::map<Float, alpha_info> >& alpha_depth_single = alpha_depths_trans[sb];
      //Calculate shadow buffer
      auto task = [&depth_shader_single, &blocks_depth, &ndc_verts_depth, &ndc_inv_w_depth,  
                   &min_block_bound_depth, &max_block_bound_depth,
                   &zbuffer_depth, &shadowbuff, &normalbuffer, &positionbuffer, &uvbuffer, 
                   &models, &alpha_depth_single, sb] (unsigned int i) {
        fill_tri_blocks(blocks_depth[i],
                        ndc_verts_depth,
                        ndc_inv_w_depth,
                        min_block_bound_depth[i],
                        max_block_bound_depth[i],
                        depth_shader_single,
                        zbuffer_depth,
                        shadowbuff,
                        normalbuffer,
                        positionbuffer,
                        uvbuffer,
                        models, true,
                        alpha_depth_single);
      };
      RcppThread::ThreadPool pool2(numbercores);
      for(int i = 0; i < nx_blocks_depth*ny_blocks_depth; i++) {
        pool2.push(task, i);
      }
      pool2.join();
      for(unsigned int j = 0; j < blocks_depth.size(); j++) {
        for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
          blocks_depth[j][model_num].clear();
        }
      }
      //Calculate transparency buffer
      for(int i = 0; i < shadowdims(0); i++) {
        for(int j = 0; j < shadowdims(1); j++) {
          for(std::map<Float, alpha_info>::reverse_iterator it = alpha_depth_single[j + shadowdims(1)*i].rbegin();
              it != alpha_depth_single[j + shadowdims(1)*i].rend(); ++it) {
            if(it->first <= zbuffer_depth(i,j)) {
              // zbuffer_depth(i,j) = it->first;
              vec4 temp_col = it->second.color;
              vec4 old_color = transparency_buffers[sb].get_color_a(i,j);
              Float d = (1 - old_color.w) * (1 - temp_col.w) ;
              old_color *= temp_col;
              old_color.w = (1-d);
              transparency_buffers[sb].set_color(i,j,old_color);
            }
          }
        }
      }
      std::fill(zbuffer_depth.begin(), zbuffer_depth.end(), std::numeric_limits<Float>::infinity() ) ;
    }
  }
  
  //Calculate Image
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<Float>::infinity() ) ;

  for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(int i = 0; i < shp.num_indices; i++) {

      int mat_num = shp.materials[i] >= 0 && shp.materials[i] < (int)shaders.size() ?
        shp.materials[i] : shaders.size()-1;
      ndc_verts[model_num][0][i] = shaders[mat_num]->vertex(i,0, shp);
      ndc_verts[model_num][1][i] = shaders[mat_num]->vertex(i,1, shp);
      ndc_verts[model_num][2][i] = shaders[mat_num]->vertex(i,2, shp);

      //Depth clamping
      ndc_verts[model_num][0][i].w = ndc_verts[model_num][0][i].w < near_clip ? near_clip : ndc_verts[model_num][0][i].w;
      ndc_verts[model_num][1][i].w = ndc_verts[model_num][1][i].w < near_clip ? near_clip : ndc_verts[model_num][1][i].w;
      ndc_verts[model_num][2][i].w = ndc_verts[model_num][2][i].w < near_clip ? near_clip : ndc_verts[model_num][2][i].w;

      //Depth clamping
      ndc_verts[model_num][0][i].w = ndc_verts[model_num][0][i].w > far_clip ? far_clip : ndc_verts[model_num][0][i].w;
      ndc_verts[model_num][1][i].w = ndc_verts[model_num][1][i].w > far_clip ? far_clip : ndc_verts[model_num][1][i].w;
      ndc_verts[model_num][2][i].w = ndc_verts[model_num][2][i].w > far_clip ? far_clip : ndc_verts[model_num][2][i].w;

      ndc_inv_w[model_num][0][i] = 1.0f/ndc_verts[model_num][0][i].w;
      ndc_inv_w[model_num][1][i] = 1.0f/ndc_verts[model_num][1][i].w;
      ndc_inv_w[model_num][2][i] = 1.0f/ndc_verts[model_num][2][i].w;

      vec3 v1 = ndc_verts[model_num][0][i] * ndc_inv_w[model_num][0][i];
      vec3 v2 = ndc_verts[model_num][1][i] * ndc_inv_w[model_num][1][i];
      vec3 v3 = ndc_verts[model_num][2][i] * ndc_inv_w[model_num][2][i];

      vec3 min_bounds = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                             fmin(v1.y,fmin(v2.y,v3.y)),
                             fmin(v1.z,fmin(v2.z,v3.z)));

      vec3 max_bounds = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                             fmax(v1.y,fmax(v2.y,v3.y)),
                             fmax(v1.z,fmax(v2.z,v3.z)));
      

      int min_x_block = std::fmax(floor(min_bounds.x / (Float)blocksize), 0);
      int min_y_block = std::fmax(floor(min_bounds.y / (Float)blocksize), 0);
      int max_x_block = std::fmin(ceil(max_bounds.x  / (Float)blocksize), nx_blocks);
      int max_y_block = std::fmin(ceil(max_bounds.y  / (Float)blocksize), ny_blocks);
      if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks && min_y_block < ny_blocks) {
        for(int j = min_x_block; j < max_x_block; j++) {
          for(int k = min_y_block; k < max_y_block; k++) {
            blocks[k + ny_blocks * j][model_num].push_back(i);
          }
        }
      }
    }
  }

  auto task = [&shaders, &models, &blocks, &ndc_verts, &ndc_inv_w,  &min_block_bound, &max_block_bound,
               &zbuffer, &image, &normalbuffer, &positionbuffer, &uvbuffer,
               &alpha_depths] (unsigned int i) {
    fill_tri_blocks(blocks[i],
                    ndc_verts,
                    ndc_inv_w,
                    min_block_bound[i],
                    max_block_bound[i],
                    shaders,
                    zbuffer,
                    image,
                    normalbuffer,
                    positionbuffer,
                    uvbuffer,
                    models, false,
                    alpha_depths);
  };
  

  RcppThread::ThreadPool pool(numbercores);
  for(int i = 0; i < nx_blocks*ny_blocks; i++) {
    pool.push(task, i);
  }
  pool.join();

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

    constexpr unsigned int noiseSize=16;
    vec3 noise[noiseSize];
    for (unsigned int i = 0; i < noiseSize; ++i) {
      noise[i] = normalize(vec3(
        spacefillr::sobol_owen_single(i,0,1) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,1,1) * 2.0f - 1.0f,
        spacefillr::sobol_owen_single(i,2,1)));
    }
    for (int x = 0; x < nx; x++) {
      for (int y = 0; y < ny; y++) {
        if (nxbuffer(x,y) == 0 && nybuffer(x,y) == 0 && nzbuffer(x,y) == 0) {
          continue;
        }
        vec3 origin(xxbuffer(x,y), yybuffer(x,y), zzbuffer(x,y));
        vec3 normal(nxbuffer(x,y), nybuffer(x,y), nzbuffer(x,y));
        normal = normalize(normal);
        int noisex = x % 4;
        int noisey = y % 4;
        vec3 rvec = noise[noisex + 8*noisey];
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
            Float sampleDepth = zzbuffer((int)offset.x, (int)offset.y);
            // range check & accumulate:
            Float rangeCheck= std::fabs(origin.z - sampleDepth) < (Float)ambient_radius ? 1.0 : 0.0;
            occlusion += (sampleDepth >= sample.z ? 1.0 : 0.0) * rangeCheck;
          }
        }
        occlusion = 1.0 - (occlusion / (Float)kernelSize);
        abuffer(x,y) = occlusion;
      }
    }
    NumericMatrix abuffer_noblur = abuffer;
    for (int x = 0; x < nx; x++) {
      for (int y = 0; y < ny; y++) {
        int uBlurSize = 4;
        Float result = 0.0;
        int counter = 0;
        vec2 hlim = vec2(Float(-uBlurSize) * 0.5);
        for (int i = 0; i < uBlurSize; ++i) {
          for (int j = 0; j < uBlurSize; ++j) {
            vec2 offset = (hlim + vec2((Float)i,(Float)j) + vec2(Float(x), Float(y)));
            if((int)offset.x >= 0 && (int)offset.x < nx && (int)offset.y >= 0 && (int)offset.y < ny) {
              result += abuffer_noblur((int)offset.x,(int)offset.y);
              counter++;
            }
          }
        }
        if(counter > 0) {
          abuffer(x,y) = result / Float(counter);
        }
      }
    }
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

  for(int i = 0; i < nx; i++) {
    for(int j = 0; j < ny; j++) {
      for(std::map<Float, alpha_info>::reverse_iterator it = alpha_depths[j + ny*i].rbegin();
          it != alpha_depths[j + ny*i].rend(); ++it) {
        if(it->first <= zbuffer(i,j)) {
          zbuffer(i,j) = it->first;
          vec4 temp_col = it->second.color;
          vec3 old_color = image.get_color(i,j);
          vec3 new_color = vec3(temp_col)*temp_col.w + vec3(old_color)*(1-temp_col.w);
          image.set_color(i,j,new_color);
          normalbuffer.set_color(i,j,it->second.normal);
          positionbuffer.set_color(i,j,it->second.position);
          uvbuffer.set_color(i,j,it->second.uv);
        }
      }
    }
  }
  
  //Load/blur environment image
  if(loaded_reflection_map) {
    if(background_sharpness != 1.0) {
      int nx_r_resize = (double)nx_r * background_sharpness;
      int ny_r_resize = (double)ny_r * background_sharpness;
      
      float* reflection_map_data_temp = new float[nx_r_resize * ny_r_resize * nn_r];
      
      stbir_resize_float_generic(main_reflection_map.reflection, nx_r, ny_r, 0, 
                         reflection_map_data_temp, nx_r_resize, ny_r_resize, 0,
                         nn_r, 0, 0, STBIR_EDGE_WRAP, STBIR_FILTER_CUBICBSPLINE, STBIR_COLORSPACE_LINEAR, NULL);
      
      stbir_resize_float_generic(reflection_map_data_temp, nx_r_resize, ny_r_resize, 0, 
                         main_reflection_map.reflection, nx_r, ny_r, 0,
                         nn_r, 0, 0, STBIR_EDGE_WRAP, STBIR_FILTER_CUBICBSPLINE, STBIR_COLORSPACE_LINEAR, NULL);
      delete[] reflection_map_data_temp;
    }
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
    for(int i = 0; i < nx; i++) {
      for(int j = 0; j < ny; j++) {
        if(std::isinf(zbuffer(i,j))) {
          Float s = (Float(i)) / Float(nx);
          Float t = (Float(j)) / Float(ny);
          vec2 uv;
          vec3 dir = glm::normalize(lower_left_corner + s * horizontal + t * vertical - origin);
          get_sphere_uv(dir,uv);
          uv.x = 1 - uv.x;
          uv.x += 0.25;
          vec3 ref_color = trivalue(uv.x,uv.y,main_reflection_map);
          image.set_color(i,j,ref_color);
        }
      }
    }
  }
  
  if(loaded_reflection_map) {
    stbi_image_free(main_reflection_map.reflection);
  }
  
  for(unsigned int j = 0; j < directional_lights.size(); j++) {
    for(int i = 0; i < depthshaders[j].size(); i++) {
      delete depthshaders[j][i];
    }
  }
  for(unsigned int i = 0; i < shaders.size(); i++) {
    delete shaders[i];
  }
  
  for(unsigned int i = 0; i < has_reflection_map.size(); i++) {
    if(has_reflection_map(i)) {
      delete[] reflection_maps[i].reflection;
    }
  }

  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b,
                      _["amb"] = abuffer, _["depth"] = zbuffer,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer,
                      _["uvx"] = uvxbuffer, _["uvy"] = uvybuffer, _["uvz"] = uvzbuffer));
}

#endif
