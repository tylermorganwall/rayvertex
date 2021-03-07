#define STB_IMAGE_IMPLEMENTATION 

#ifndef RAYRASTERH
#define RAYRASTERH

// // [[Rcpp::depends(RcppParallel)]]
#include "Rcpp.h"
// #include <RcppParallel.h>
// #include <RcppParallel/TinyThread.h>

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
// [[Rcpp::depends(RcppThread)]]
#include "RcppThread.h"

#include "filltri.h"

using namespace Rcpp;
// using namespace RcppParallel;


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

// tbb::mutex lock;
// 
// struct FillTriParallel : public Worker {
//   // source vector
//   RMatrix<double> vertices;
//   RMatrix<int> indices;
//   RMatrix<double> zbuffer;
//   RMatrix<double> rbuffer;
//   RMatrix<double> bbuffer;
//   RMatrix<double> gbuffer;
//   IShader* shader;
//   
//   // constructors
//   FillTriParallel(NumericMatrix vertices,
//                   IntegerMatrix indices,
//                   NumericMatrix zbuffer,
//                   NumericMatrix rbuffer,
//                   NumericMatrix gbuffer,
//                   NumericMatrix bbuffer,
//                   IShader* shader) : vertices(vertices), 
//                   indices(indices), zbuffer(zbuffer), rbuffer(rbuffer),
//                   bbuffer(bbuffer), gbuffer(gbuffer), shader(shader) {}
//   FillTriParallel(const FillTriParallel& tris, Split) : 
//     vertices(tris.vertices), 
//     indices(tris.indices), zbuffer(tris.zbuffer), rbuffer(tris.rbuffer),
//     bbuffer(tris.bbuffer), gbuffer(tris.gbuffer), shader(tris.shader) {}
//   
//   void operator()(std::size_t begin, std::size_t end) {
//     for(size_t i = begin; i < end; i++) {
//       fill_tri_raw(i, shader, zbuffer, rbuffer, gbuffer, bbuffer, lock);
//     }
//   }
//   
//   void join(const FillTriParallel& rhs, Split) {
//     // for(int i = 0; i < zbuffer.nrow(); i++) {
//       // for(int j = 0; j <  zbuffer.ncol(); j++) {
//         // while(!lock.try_lock())
//         // if(rhs.zbuffer(i,j) < zbuffer(i,j)) {
//         //   rbuffer(i,j) = rhs.rbuffer(i,j);
//         //   gbuffer(i,j) = rhs.gbuffer(i,j);
//         //   bbuffer(i,j) = rhs.bbuffer(i,j);
//         //   zbuffer(i,j) = rhs.zbuffer(i,j);
//         // }
//         // lock.unlock();
//       // }
//     // }
//   };
// };

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
  
  int blocksize = 32;
  std::vector<std::vector<vec4> > ndc_verts(3, std::vector<vec4>(n));
  std::vector<std::vector<float> > ndc_inv_w(3, std::vector<float>(n));
  std::vector<vec3> min_bounds(n);
  std::vector<vec3> max_bounds(n);
  
  std::vector<std::vector<int> > blocks;
  std::vector<vec2> min_block_bound;
  std::vector<vec2> max_block_bound;
  
  int nx_blocks = ceil((float)nx/(float)blocksize);
  int ny_blocks = ceil((float)ny/(float)blocksize);
  
  for(int i = 0; i < nx; i += blocksize) {
    for(int j = 0; j < ny; j += blocksize) {
      std::vector<int> temp;
      blocks.push_back(temp);
      min_block_bound.push_back(vec2(i,j));
      max_block_bound.push_back(vec2(std::min(i+blocksize,nx),std::min(j+blocksize,ny)));
    }
  }
  
  for(int i = 0; i < n; i++) {
    ndc_verts[0][i] = shader->vertex(i,0);
    ndc_verts[1][i] = shader->vertex(i,1);
    ndc_verts[2][i] = shader->vertex(i,2);
    
    ndc_inv_w[0][i] = 1.0f/ndc_verts[0][i].w;
    ndc_inv_w[1][i] = 1.0f/ndc_verts[1][i].w;
    ndc_inv_w[2][i] = 1.0f/ndc_verts[2][i].w;
    
    ndc_verts[0][i] *= ndc_inv_w[0][i];
    ndc_verts[1][i] *= ndc_inv_w[1][i];
    ndc_verts[2][i] *= ndc_inv_w[2][i];
    
    min_bounds[i] = vec3(fmin(ndc_verts[0][i].x,fmin(ndc_verts[1][i].x,ndc_verts[2][i].x)),
                         fmin(ndc_verts[0][i].y,fmin(ndc_verts[1][i].y,ndc_verts[2][i].y)),
                         fmin(ndc_verts[0][i].z,fmin(ndc_verts[1][i].z,ndc_verts[2][i].z)));
    
    max_bounds[i] = vec3(fmax(ndc_verts[0][i].x,fmax(ndc_verts[1][i].x,ndc_verts[2][i].x)),
                         fmax(ndc_verts[0][i].y,fmax(ndc_verts[1][i].y,ndc_verts[2][i].y)),
                         fmax(ndc_verts[0][i].z,fmax(ndc_verts[1][i].z,ndc_verts[2][i].z)));
    
    int min_x_block = floor(min_bounds[i].x / (float)blocksize);
    int min_y_block = floor(min_bounds[i].y / (float)blocksize);
    int max_x_block =  ceil(max_bounds[i].x / (float)blocksize);
    int max_y_block =  ceil(max_bounds[i].y / (float)blocksize);
    if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks && min_y_block < ny_blocks) {
      for(int j = min_x_block; j < max_x_block; j++) {
        for(int k = min_y_block; k < max_y_block; k++) {
          blocks[k + nx_blocks * j].push_back(i);
        }
      }
    }
  }
  
  // for(int i = 0; i < n; i++) {
  //   fill_tri(i, shader.get(), zbuffer, image, normalbuffer, positionbuffer);
  // }
  
  
  auto sp = shader.get();
  auto task = [sp, &blocks, &ndc_verts, &ndc_inv_w,  &min_block_bound, &max_block_bound,
               &zbuffer, &image, &normalbuffer, &positionbuffer] (unsigned int i) {
                 std::unique_ptr<IShader> shader;
    fill_tri_blocks(blocks[i],
                    ndc_verts,
                    ndc_inv_w,
                    min_block_bound[i],
                    max_block_bound[i],
                    sp, 
                    zbuffer, 
                    image, 
                    normalbuffer, 
                    positionbuffer);
  };

  RcppThread::ThreadPool pool(8);
  for(int i = 0; i < nx_blocks*ny_blocks; i++) {
    pool.push(task, i);
  }
  pool.join();

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