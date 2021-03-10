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

#include "material.h"

#include "filltri.h"

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
List rasterize(List mesh,
               int nx, int ny,
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
               IntegerVector typevals,
               bool has_shadow_map,
               bool calc_ambient, 
               bool tbn,
               float ambient_radius,
               float shadow_map_bias,
               int numbercores,
               int max_indices,
               LogicalVector has_normals_vec,
               LogicalVector has_tex_vec,
               
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
  
  Mat vp = glm::scale(glm::translate(Mat(1.0f),
                                     vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)),
                                     vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  Mat M = vp*lightProjection*lightView*Model;
  Mat uniform_Mshadow_ = M*glm::inverse(vp*Projection*View*Model);
  
  ///
  //Parse mesh3d
  //
  
  //Start by generating a shader for every material
  std::vector<material_info> mat_info;
  std::vector<IShader*> shaders;
  
  List materials = as<List>(mesh["materials"]);
  int number_materials = materials.size();
  for(int i = 0; i < number_materials; i++) {
    List single_material = as<List>(materials(i));
    NumericVector ambient = as<NumericVector>(single_material["ambient"]);
    NumericVector diffuse = as<NumericVector>(single_material["diffuse"]);
    NumericVector specular = as<NumericVector>(single_material["specular"]);
    NumericVector transmittance = as<NumericVector>(single_material["transmittance"]);
    NumericVector emission = as<NumericVector>(single_material["emission"]);
    float shininess = as<float>(single_material["shininess"]);
    float ior = as<float>(single_material["ior"]);
    float dissolve = as<float>(single_material["dissolve"]);
    float illum = as<float>(single_material["illum"]);
    String ambient_texname = as<String>(single_material["ambient_texname"]);
    String diffuse_texname = as<String>(single_material["diffuse_texname"]);
    String specular_texname = as<String>(single_material["specular_texname"]);
    String normal_texname = as<String>(single_material["normal_texname"]);
    String emissive_texname = as<String>(single_material["emissive_texname"]);
    bool has_norms = has_normals_vec(i);
    bool has_tex = has_tex_vec(i);
    
    material_info temp = {
      vec3(ambient(0),ambient(1),ambient(2)),
      vec3(diffuse(0),diffuse(1),diffuse(2)),
      vec3(specular(0),specular(1),specular(2)),
      vec3(transmittance(0),transmittance(1),transmittance(2)),
      vec3(emission(0),emission(1),emission(2)),
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
      emission_intensity,
      diffuse_intensity,
      specular_intensity,
      has_norms,
      has_tex
    };
    mat_info.push_back(temp);
    
    IShader* shader;
    int type = typevals(i);
    if(type == 1) {
      shader = new GouraudShader(Model, Projection, View, viewport,
                                 glm::normalize(light_dir), 
                                 shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                 shadow_map_bias,mat_info[i]);
    } else if (type == 2) {
      shader = new DiffuseShader(Model, Projection, View, viewport,
                                 glm::normalize(light_dir), 
                                 shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                 shadow_map_bias,mat_info[i]);
    } else if (type == 3) {
      shader = new PhongShader(Model, Projection, View, viewport,
                               glm::normalize(light_dir), 
                               shadowbuffer, uniform_Mshadow_, has_shadow_map,
                               shadow_map_bias,mat_info[i]);
    } else if (type == 4) {
      shader = new DiffuseNormalShader(Model, Projection, View, viewport,
                                       glm::normalize(light_dir), 
                                       shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                       shadow_map_bias,mat_info[i]);
    } else if (type == 5) {
      shader = new DiffuseShaderTangent(Model, Projection, View, viewport,
                                        glm::normalize(light_dir), 
                                        shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                        shadow_map_bias,mat_info[i]);
    } else if (type == 6) {
      shader = new PhongNormalShader(Model, Projection, View, viewport,
                                     glm::normalize(light_dir), 
                                     shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                     shadow_map_bias,mat_info[i]);
    } else if (type == 7) {
      shader = new PhongShaderTangent(Model, Projection, View, viewport,
                                      glm::normalize(light_dir),
                                      shadowbuffer, uniform_Mshadow_, has_shadow_map,
                                      shadow_map_bias,mat_info[i]);
    } else {
      throw std::runtime_error("shader not recognized");
    }
    shaders.push_back(shader);
  }
  
  //Default material initialize
  
  material_info default_mat = {
    vec3(ambient_color(0),ambient_color(1),ambient_color(2)),
    vec3(model_color(0),model_color(1),model_color(2)),
    vec3(0.8),
    vec3(1.0),
    vec3(0.0),
    32,
    1.0,
    0.0,
    0.0,
    "",
    "",
    "",
    "",
    "",
    max_indices, //Maybe
    0.0,
    1.0,
    1.0,
    false,
    false
  };
  
  IShader* default_shader = new DiffuseShader(Model, Projection, View, viewport,
                             glm::normalize(light_dir), 
                             shadowbuffer, uniform_Mshadow_, has_shadow_map,
                             shadow_map_bias, default_mat);
  shaders.push_back(default_shader);
  
  List shapes = as<List>(mesh["shapes"]);
  int number_shapes = shapes.size();

  std::vector<ModelInfo> models;

  std::vector<std::vector<std::vector<vec4>  > > ndc_verts;
  std::vector<std::vector<std::vector<float> > > ndc_inv_w;
  std::vector<std::vector<vec3> > min_bounds;
  std::vector<std::vector<vec3> > max_bounds;
  
  for(int i = 0; i < number_shapes; i++) {
    List single_shape = as<List>(shapes(i));
    NumericMatrix shape_verts = as<NumericMatrix>(single_shape["positions"]);
    NumericMatrix shape_normals = as<NumericMatrix>(single_shape["normals"]);
    NumericMatrix shape_texcoords = as<NumericMatrix>(single_shape["texcoords"]);
    IntegerMatrix shape_inds = as<IntegerMatrix>(single_shape["indices"]);
    IntegerVector shape_materials = as<IntegerVector>(single_shape["material_ids"]);
    
    int n = shape_inds.nrow();
    
    ndc_verts.push_back(std::vector<std::vector<vec4>  >(3, std::vector<vec4>(n)));
    ndc_inv_w.push_back(std::vector<std::vector<float> >(3, std::vector<float>(n)));
    min_bounds.push_back(std::vector<vec3>(n));
    max_bounds.push_back(std::vector<vec3>(n));
    
    
    //Create model object
    ModelInfo model(shape_verts, shape_inds, shape_texcoords, shape_normals, shape_materials,
                    has_normals_vec(i), has_tex_vec(i), tbn);
    models.push_back(model);
  }

  
  //Set up blocks
  int blocksize = 4;
  
  //First index model, then block, then index
  std::vector<std::vector<std::vector<int> > > blocks(models.size());
  
  std::vector<vec2> min_block_bound;
  std::vector<vec2> max_block_bound;
  
  int nx_blocks = ceil((float)nx/(float)blocksize);
  int ny_blocks = ceil((float)ny/(float)blocksize);
  
  std::vector<std::vector<int> > single_model_blocks;
  for(int i = 0; i < nx; i += blocksize) {
    for(int j = 0; j < ny; j += blocksize) {
      for(int k = 0; k < models.size(); k++) {
        std::vector<int> temp;
        blocks[k].push_back(temp);
      }
      min_block_bound.push_back(vec2(i,j));
      max_block_bound.push_back(vec2(std::min(i+blocksize,nx),std::min(j+blocksize,ny)));
    }
  }

  IShader* depthshader = new DepthShader(Model, lightProjection, lightView, viewport, light_dir, max_indices);

  if(has_shadow_map) {
    for(int model_num = 0; model_num < models.size(); model_num++ ) {
      ModelInfo &shp = models[model_num];
      for(int i = 0; i < shp.num_indices; i++) {
        ndc_verts[model_num][0][i] = depthshader->vertex(i,0, shp);
        ndc_verts[model_num][1][i] = depthshader->vertex(i,1, shp);
        ndc_verts[model_num][2][i] = depthshader->vertex(i,2, shp);
        
        ndc_inv_w[model_num][0][i] = 1.0f/ndc_verts[model_num][0][i].w;
        ndc_inv_w[model_num][1][i] = 1.0f/ndc_verts[model_num][1][i].w;
        ndc_inv_w[model_num][2][i] = 1.0f/ndc_verts[model_num][2][i].w;
        
        vec3 v1 = ndc_verts[model_num][0][i] * ndc_inv_w[model_num][0][i];
        vec3 v2 = ndc_verts[model_num][1][i] * ndc_inv_w[model_num][1][i];
        vec3 v3 = ndc_verts[model_num][2][i] * ndc_inv_w[model_num][2][i];
        
        min_bounds[model_num][i] = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                             fmin(v1.y,fmin(v2.y,v3.y)),
                             fmin(v1.z,fmin(v2.z,v3.z)));
        
        max_bounds[model_num][i] = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                             fmax(v1.y,fmax(v2.y,v3.y)),
                             fmax(v1.z,fmax(v2.z,v3.z)));
        
        int min_x_block = std::fmax(floor(min_bounds[model_num][i].x / (float)blocksize),0);
        int min_y_block = std::fmax(floor(min_bounds[model_num][i].y / (float)blocksize),0);
        int max_x_block = std::fmin(ceil(max_bounds[model_num][i].x / (float)blocksize), nx_blocks);
        int max_y_block = std::fmin(ceil(max_bounds[model_num][i].y / (float)blocksize), ny_blocks);
        if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks && min_y_block < ny_blocks) {
          for(int j = min_x_block; j < max_x_block; j++) {
            for(int k = min_y_block; k < max_y_block; k++) {
              blocks[model_num][k + ny_blocks * j].push_back(i); 
            }
          }
        }
      }
    }
    
    std::vector<IShader* > depth_vec;
    for(int i = 0; i < shaders.size(); i++ ) {
      depth_vec.push_back(depthshader);
    }
    auto task = [&depth_vec, &blocks, &ndc_verts, &ndc_inv_w,  &min_block_bound, &max_block_bound,
                 &zbuffer, &shadowbuffer, &normalbuffer, &positionbuffer, &models] (unsigned int i) {
      fill_tri_blocks(blocks,
                      ndc_verts,
                      ndc_inv_w,
                      min_block_bound[i],
                      max_block_bound[i],
                      depth_vec,
                      zbuffer,
                      shadowbuffer,
                      normalbuffer,
                      positionbuffer,
                      models,
                      i);
    };
    
    RcppThread::ThreadPool pool2(numbercores);
    for(int i = 0; i < nx_blocks*ny_blocks; i++) {
      pool2.push(task, i);
    }
    pool2.join();

    //Clear out block info from depth
    for(int j = 0; j < nx_blocks; j++) {
      for(int k = 0; k < ny_blocks; k++) {
        blocks[k + ny_blocks * j].clear();
      }
    }
  }

  //Calculate Image
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;

  for(int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(int i = 0; i < shp.num_indices; i++) {
      int mat_num = shp.materials[i] != -1 ? shp.materials[i] : shaders.size()-1;
      ndc_verts[model_num][0][i] = shaders[mat_num]->vertex(i,0, shp);
      ndc_verts[model_num][1][i] = shaders[mat_num]->vertex(i,1, shp);
      ndc_verts[model_num][2][i] = shaders[mat_num]->vertex(i,2, shp);
  
      ndc_inv_w[model_num][0][i] = 1.0f/ndc_verts[model_num][0][i].w;
      ndc_inv_w[model_num][1][i] = 1.0f/ndc_verts[model_num][1][i].w;
      ndc_inv_w[model_num][2][i] = 1.0f/ndc_verts[model_num][2][i].w;
  
      vec3 v1 = ndc_verts[model_num][0][i] * ndc_inv_w[model_num][0][i];
      vec3 v2 = ndc_verts[model_num][1][i] * ndc_inv_w[model_num][1][i];
      vec3 v3 = ndc_verts[model_num][2][i] * ndc_inv_w[model_num][2][i];
  
      min_bounds[model_num][i] = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                           fmin(v1.y,fmin(v2.y,v3.y)),
                           fmin(v1.z,fmin(v2.z,v3.z)));
  
      max_bounds[model_num][i] = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                           fmax(v1.y,fmax(v2.y,v3.y)),
                           fmax(v1.z,fmax(v2.z,v3.z)));
  
      int min_x_block = std::fmax(floor(min_bounds[model_num][i].x / (float)blocksize),0);
      int min_y_block = std::fmax(floor(min_bounds[model_num][i].y / (float)blocksize),0);
      int max_x_block = std::fmin(ceil(max_bounds[model_num][i].x / (float)blocksize), nx_blocks);
      int max_y_block = std::fmin(ceil(max_bounds[model_num][i].y / (float)blocksize), ny_blocks);
      if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks && min_y_block < ny_blocks) {
        for(int j = min_x_block; j < max_x_block; j++) {
          for(int k = min_y_block; k < max_y_block; k++) {
            blocks[model_num][k + ny_blocks * j].push_back(i);
          }
        }
      }
    }
  }

  auto task = [&shaders, &models, &blocks, &ndc_verts, &ndc_inv_w,  &min_block_bound, &max_block_bound,
               &zbuffer, &image, &normalbuffer, &positionbuffer] (unsigned int i) {
    fill_tri_blocks(blocks,
                    ndc_verts,
                    ndc_inv_w,
                    min_block_bound[i],
                    max_block_bound[i],
                    shaders,
                    zbuffer,
                    image,
                    normalbuffer,
                    positionbuffer,
                    models,i);
  };

  RcppThread::ThreadPool pool(numbercores);
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
  
  delete depthshader;
  for(int i = 0; i < shaders.size(); i++) {
    delete shaders[i];
  }

  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b, _["amb"] = abuffer, _["depth"] = zbuffer,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer));
}

#endif