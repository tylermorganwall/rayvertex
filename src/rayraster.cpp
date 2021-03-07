#define STB_IMAGE_IMPLEMENTATION 

#ifndef RAYRASTERH
#define RAYRASTERH

#include "Rcpp.h"

#include <functional>
#include <algorithm>
#include <utility>
#include "stb_image.h"
#include <memory>
#include "glm.hpp"
#include "gtc/matrix_transform.hpp"

#include "shaders.h"
#include "rayimage.h"
#include "model.h"
#include "single_sample.h"

using namespace Rcpp;




inline vec3 clamp(const vec3& c, float clamplow, float clamphigh) {
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

float edgeFunction(const vec3 &a, const vec3 &b, const vec3 &c) { 
  return (c.x - a.x) * (b.y - a.y) - (c.y - a.y) * (b.x - a.x); 
} 

void fill_tri(int vertex,
              IShader* shader,
              NumericMatrix &zbuffer, 
              rayimage& image, rayimage& normal_buffer,
              rayimage& position_buffer) { 
  vec4 v1_ndc = shader->vertex(vertex,0);
  vec4 v2_ndc = shader->vertex(vertex,1);
  vec4 v3_ndc = shader->vertex(vertex,2);
  
  vec3 v1 = v1_ndc/v1_ndc.w;
  vec3 v2 = v2_ndc/v2_ndc.w;
  vec3 v3 = v3_ndc/v3_ndc.w;
  
  //Backface culling
  if(cross(v2-v1, v3-v2).z > 0) {
    vec3 bound_min = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                          fmin(v1.y,fmin(v2.y,v3.y)),
                          fmin(v1.z,fmin(v2.z,v3.z)));
    vec3 bound_max = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                          fmax(v1.y,fmax(v2.y,v3.y)),
                          fmax(v1.z,fmax(v2.z,v3.z)));
    
    int nx = image.width();
    int ny = image.height();
    
    int xmin =  std::min(std::max((int)floor(bound_min.x),0 ), 0);
    int xmax =  std::max(std::min((int)ceil(bound_max.x), nx),nx);
    int ymin =  std::min(std::max((int)floor(bound_min.y),0), 0);
    int ymax =  std::max(std::min((int)ceil(bound_max.y), ny ),ny);
    float area = edgeFunction(v3, v2, v1); 
    float inv_area = 1/area;
    
    vec3 color;
    vec3 position;
    vec3 normal;
    vec3 bc_clip;
    vec3 bc;
    
    float p_step_32 = -(v2.x-v3.x);
    float p_step_13 = -(v3.x-v1.x);
    float p_step_21 = -(v1.x-v2.x);
    
    float pi_step_32 = (v2.y-v3.y);
    float pi_step_13 = (v3.y-v1.y);
    float pi_step_21 = (v1.y-v2.y);
    vec3 p  = vec3((float)xmin + 0.5f, (float)ymin + 0.5f, 0.0f);
    
    float w1_init = edgeFunction(v3, v2, p);
    float w2_init = edgeFunction(v1, v3, p);
    float w3_init = edgeFunction(v2, v1, p);
    
    for (uint32_t i = xmin; i < xmax; i++) {
      float w1 = w1_init;
      float w2 = w2_init;
      float w3 = w3_init;
      for (uint32_t j = ymin; j < ymax; j++) {
        if (w1 >= 0 && w2 >= 0 && w3 >= 0) {
          bc = vec3(w1,w2,w3)*inv_area;
          bc_clip    = vec3(bc.x/v1_ndc.w, bc.y/v2_ndc.w, bc.z/v3_ndc.w);
          bc_clip /= (bc_clip.x+bc_clip.y+bc_clip.z);
          p.z = v1.z * bc_clip.x + v2.z * bc_clip.y + v3.z * bc_clip.z;
          if(p.z > zbuffer(i,j)) continue;

          bool discard = shader->fragment(bc_clip, color, position, normal);
          if(!discard) {
            zbuffer(i,j) = p.z;
            image.set_color(i,j,color);
            normal_buffer.set_color(i,j,normal);
            position_buffer.set_color(i,j,position);
          }
        }
        w1 += p_step_32;
        w2 += p_step_13;
        w3 += p_step_21;
      }
      w1_init += pi_step_32;
      w2_init += pi_step_13;
      w3_init += pi_step_21;
    }
  }
}

template<class T>
inline T lerp(float t, T v1, T v2) {
  return((1-t) * v1 + t * v2);
}

void print_mat(Mat m) {
  m = glm::transpose(m);
  Rcpp::Rcout.precision(5);
  Rcpp::Rcout << std::fixed << m[0][0] << " " << m[0][1] << " " << m[0][2] << " " << m[0][3] << "\n";
  Rcpp::Rcout << std::fixed << m[1][0] << " " << m[1][1] << " " << m[1][2] << " " << m[1][3] << "\n";
  Rcpp::Rcout << std::fixed << m[2][0] << " " << m[2][1] << " " << m[2][2] << " " << m[2][3] << "\n";
  Rcpp::Rcout << std::fixed << m[3][0] << " " << m[3][1] << " " << m[3][2] << " " << m[3][3] << "\n";
}

// [[Rcpp::export]]
List rasterize(NumericMatrix verts, IntegerMatrix inds, 
               NumericMatrix texcoords, NumericMatrix normals,
               int nx, int ny,
               CharacterVector texture_location,     
               CharacterVector normal_texture_location, 
               CharacterVector specular_texture_location,
               CharacterVector emissive_texture_location,
               NumericVector model_color,
               NumericVector lookfrom,
               NumericVector lookat,
               float fov,
               NumericVector light_direction,
               NumericVector ambient_color, 
               float exponent, 
               float specular_intensity, 
               float diffuse_intensity, 
               float emission_intensity,
               int type,
               bool has_normals,
               bool has_texcoords,
               bool has_texture,
               bool has_normal_texture,
               bool has_specular_texture,
               bool has_emissive_texture,
               bool has_shadow_map,
               bool calc_ambient, 
               bool tbn,
               float ambient_radius,
               float shadow_map_bias,
               float near_clip = 0.1,
               float  far_clip = 100) {
  vec3 eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));    //lookat
  vec3 cam_up = vec3(0.,1.,0.);
  vec3 color(model_color(0),model_color(1),model_color(2));
  vec3 ambient(ambient_color(0),ambient_color(1),ambient_color(2)); 
  vec3 light_dir(light_direction(0),light_direction(1),light_direction(2));
  
  if(std::fabs(glm::dot(eye-center,cam_up)) == 1) {
    cam_up = vec3(0.f,0.f,1.0f);
  }
  
  //Turn off gamma correction or get seams in textures/normal maps
  stbi_ldr_to_hdr_gamma(1.0f);
    
  glm::mat4 View       = glm::lookAt(eye, center, cam_up);
  glm::mat4 Model      = glm::translate(Mat(1.0f), vec3(0.0f, 0.0f, 0.0f));
  glm::mat4 Projection = glm::perspective(glm::radians(fov), 
                                          (float)nx / (float)ny, 
                                          near_clip, 
                                          far_clip);
  vec4 viewport(0.0f, 0.0f, (float)nx-1, (float)ny-1);

  //Initialize output matrices
  NumericMatrix r(nx,ny);
  NumericMatrix g(nx,ny);
  NumericMatrix b(nx,ny);
  
  //Create buffers
  rayimage image(r,g,b,nx,ny);
  
  //Depth buffer
  NumericMatrix zbuffer(nx,ny);
  NumericMatrix sbuffer(nx,ny);
  NumericMatrix abuffer(nx,ny);
  
  std::fill(abuffer.begin(), abuffer.end(), 1.0 ) ;
  
  
  //Position space buffer
  NumericMatrix xxbuffer(nx,ny);
  NumericMatrix yybuffer(nx,ny);
  NumericMatrix zzbuffer(nx,ny);
  
  //Normal space buffer
  NumericMatrix nxbuffer(nx,ny);
  NumericMatrix nybuffer(nx,ny);
  NumericMatrix nzbuffer(nx,ny);

  rayimage shadowbuffer(sbuffer,sbuffer,sbuffer,nx,ny);
  rayimage ambientbuffer(abuffer,abuffer,abuffer,nx,ny);
  rayimage positionbuffer(xxbuffer,yybuffer,zzbuffer,nx,ny);
  rayimage normalbuffer(nxbuffer,nybuffer,nzbuffer,nx,ny);
  
  
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;
  
  //Load textures
  float* texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;
  
  int nx_t, ny_t, nn_t, nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et = 0;
  
  if(has_texture) {
    texture = stbi_loadf(texture_location(0), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(has_normal_texture) {
    normal_texture = stbi_loadf(normal_texture_location(0), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(has_specular_texture) {
    specular_texture = stbi_loadf(specular_texture_location(0), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(has_emissive_texture) {
    emissive_texture = stbi_loadf(emissive_texture_location(0), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  int n = inds.nrow();
  int cols = inds.ncol();
  if(cols < 2) {
    throw std::runtime_error("Too few columns in index matrix");
  }
  
  
  //Create model object
  ModelInfo model(verts, inds, texcoords, normals,
                  texture, normal_texture, specular_texture, emissive_texture,
                  ambient, exponent, specular_intensity, diffuse_intensity, emission_intensity,
                  nx_t, ny_t, nn_t,
                  nx_nt, ny_nt, nn_nt,
                  nx_st, ny_st, nn_st,
                  nx_et, ny_et, nn_et,
                  has_normals, has_texcoords,
                  has_texture, has_normal_texture, has_specular_texture, has_emissive_texture,
                  color, tbn);

  //Calculate Shadow Map
  float near_plane = 1.0f, far_plane = 7.5f;
  vec3 light_up = vec3(0.,1.,0.);
  if(std::fabs(glm::dot(light_up,glm::normalize(light_dir))) == 1) {
    light_up = vec3(0.f,0.f,1.0f);
  }
  //Change to bounding box scene
  
  glm::mat4 lightProjection = glm::ortho(-2.0f, 2.0f, -2.0f, 2.0f, near_plane, far_plane);
  glm::mat4 lightView = glm::lookAt(light_dir,
                                    glm::vec3( 0.0f, 0.0f,  0.0f),
                                    light_up);
  glm::mat4 lightSpaceMatrix = lightProjection * lightView;

  std::unique_ptr<IShader> depthshader(new DepthShader(Model, lightProjection, lightView, viewport,
                                                       light_dir, model));
  if(has_shadow_map) {
    for(int i = 0; i < n; i++) {
      fill_tri(i, depthshader.get(), zbuffer, shadowbuffer, normalbuffer, positionbuffer);
    }
  }
  
  Mat vp = glm::scale(glm::translate(Mat(1.0f),
                      vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)),
                      vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  Mat M = vp*lightProjection*lightView*Model;
  Mat uniform_Mshadow_ = M*glm::inverse(vp*Projection*View*Model);

  //Calculate Image
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;
  std::unique_ptr<IShader> shader;
  
  if(type == 1) {
    shader = std::unique_ptr<IShader>(new GouraudShader(Model, Projection, View, viewport,
                                                    glm::normalize(light_dir), model,
                                                    shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                    shadow_map_bias));
  } else if (type == 2) {
    shader = std::unique_ptr<IShader>(new DiffuseShader(Model, Projection, View, viewport,
                                                        glm::normalize(light_dir), model,
                                                        shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                        shadow_map_bias));
  } else if (type == 3) {
    shader = std::unique_ptr<IShader>(new PhongShader(Model, Projection, View, viewport,
                                                      glm::normalize(light_dir), model,
                                                      shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                      shadow_map_bias));
  } else if (type == 4) {
    shader = std::unique_ptr<IShader>(new DiffuseNormalShader(Model, Projection, View, viewport,
                                                      glm::normalize(light_dir), model,
                                                      shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                      shadow_map_bias));
  } else if (type == 5) {
    shader = std::unique_ptr<IShader>(new DiffuseShaderTangent(Model, Projection, View, viewport,
                                                              glm::normalize(light_dir), model,
                                                              shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                              shadow_map_bias));
  } else if (type == 6) {
    shader = std::unique_ptr<IShader>(new PhongNormalShader(Model, Projection, View, viewport,
                                                            glm::normalize(light_dir), model,
                                                            shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                            shadow_map_bias));
  } else if (type == 7) {
    shader = std::unique_ptr<IShader>(new PhongShaderTangent(Model, Projection, View, viewport,
                                                            glm::normalize(light_dir), model,
                                                            shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                                            shadow_map_bias));
  } else {
    throw std::runtime_error("shader not recognized");
  }
  // int n_core = 4;
  // List multicore_depth_buffers(n_core);
  // List multicore_r_buffers(n_core);
  // List multicore_g_buffers(n_core);
  // List multicore_b_buffers(n_core);
  // 
  // for(int i = 0; i < n_core; i++) {
  //   NumericMatrix temp(nx,ny);
  //   std::fill(temp.begin(), temp.end(), std::numeric_limits<float>::infinity() ) ;
  //   multicore_depth_buffers(i) = temp;
  //   multicore_r_buffers(i) = NumericMatrix(nx,ny);
  //   multicore_g_buffers(i) = NumericMatrix(nx,ny);
  //   multicore_b_buffers(i) = NumericMatrix(nx,ny);
  // }
  for(int i = 0; i < n; i++) {
    fill_tri(i, shader.get(), zbuffer, image, normalbuffer, positionbuffer);
  }

  //Ambient occlusion
  if(calc_ambient) {
    int kernelSize=64;
    vec3 kernel[kernelSize];
    for (int i = 0; i < kernelSize; ++i) {
      kernel[i] = normalize(vec3(
        spacefillr::sobol_single(i,0,0) * 2.0f - 1.0f,
        spacefillr::sobol_single(i,1,0) * 2.0f - 1.0f,
        spacefillr::sobol_single(i,2,0)));
      float scale = float(i) / float(kernelSize);
      scale = lerp(0.1f, 1.0f, scale * scale);
      kernel[i] *= scale;
    }
    
    int noiseSize=16;
    vec3 noise[noiseSize];
    for (int i = 0; i < noiseSize; ++i) {
      noise[i] = normalize(vec3(
        spacefillr::sobol_single(i,0,1) * 2.0f - 1.0f,
        spacefillr::sobol_single(i,1,1) * 2.0f - 1.0f,
        spacefillr::sobol_single(i,2,1)));
    }
    float uRadius = 1;
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
        vec3 rvec = noise[noisex + 4*noisey];
        vec3 tangent = normalize(rvec - normal * dot(rvec, normal));
        vec3 bitangent = cross(normal, tangent);
        glm::mat3 tbn{tangent, bitangent, normal};
        float occlusion = 0.0;
        for (int i = 0; i < kernelSize; ++i) {
          // get sample position:
          vec3 sample = tbn * kernel[i];
          sample = sample * ambient_radius + origin;
          // project sample position:
          vec4 offset = vec4(sample, 1.0);
          offset = vp * Projection * offset;
          offset /= offset.w;
          
          if((int)offset.x >= 0 && (int)offset.x < nx && (int)offset.y >= 0 && (int)offset.y < ny) {
            float sampleDepth = zzbuffer((int)offset.x, (int)offset.y);
            // range check & accumulate:
            float rangeCheck= std::fabs(origin.z - sampleDepth) < ambient_radius ? 1.0 : 0.0;
            occlusion += (sampleDepth >= sample.z ? 1.0 : 0.0) * rangeCheck;
          }
        }
        occlusion = 1.0 - (occlusion / (float)kernelSize);
        abuffer(x,y) = occlusion;
      }
    }
    NumericMatrix abuffer_noblur = abuffer;
    for (int x = 0; x < nx; x++) {
      for (int y = 0; y < ny; y++) {
        int uBlurSize = 4;
        float result = 0.0;
        int counter = 0;
        vec2 hlim = vec2(float(-uBlurSize) * 0.5);
        for (int i = 0; i < uBlurSize; ++i) {
          for (int j = 0; j < uBlurSize; ++j) {
            vec2 offset = (hlim + vec2((float)i,(float)j) + vec2(float(x), float(y)));
            if((int)offset.x >= 0 && (int)offset.x < nx && (int)offset.y >= 0 && (int)offset.y < ny) {
              result += abuffer_noblur((int)offset.x,(int)offset.y);
              counter++;
            }
          }
        }
        if(counter > 0) {
          abuffer(x,y) = result / float(counter);
        }
      }
    }
  }
  
  
  //Free memory
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
  

  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b, _["amb"] = abuffer, _["depth"] = zbuffer,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer));
}

#endif