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
               LogicalVector has_texture,  
               LogicalVector has_normal_texture,
               LogicalVector has_specular_texture,
               LogicalVector has_emissive_texture,
               int block_size,
               bool use_default_material,
               bool override_exponent,
               float near_clip,
               float  far_clip,
               float shadow_map_intensity,
               NumericVector bounds,
               IntegerVector shadowdims) {
  //Convert R vectors to glm::vec3
  vec3 eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));    //lookat
  vec3 cam_up = vec3(0.0,1.0f,0.0);
  vec3 color(model_color(0),model_color(1),model_color(2));
  vec3 ambient(ambient_color(0),ambient_color(1),ambient_color(2)); 
  vec3 light_dir(light_direction(0),light_direction(1),light_direction(2));
 
  //Account for colinear camera direction/cam_up vectors
  if(glm::length(glm::cross(eye-center,cam_up)) == 0) {
    cam_up = vec3(0.f,0.f,1.0f);
  }
  
  //Turn off gamma correction or get seams in textures/normal maps
  stbi_ldr_to_hdr_gamma(1.0f);
    
  //Generate MVP matrices
  Mat View       = glm::lookAt(eye, center, cam_up);
  Mat Model      = glm::translate(Mat(1.0f), vec3(0.0f, 0.0f, 0.0f));
  Mat Projection = glm::perspective(glm::radians(fov), 
                                          (float)nx / (float)ny, 
                                          near_clip, 
                                          far_clip);
  vec4 viewport(0.0f, 0.0f, (float)nx-1, (float)ny-1);
  vec4 viewport_depth(0.0f, 0.0f, (float)shadowdims(0)-1, (float)shadowdims(1)-1);
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

  //Initialize rayimage buffers
  rayimage shadowbuffer(sbuffer,sbuffer,sbuffer,shadowdims(0),shadowdims(1),shadow_map_intensity);
  rayimage ambientbuffer(abuffer,abuffer,abuffer,nx,ny);
  rayimage positionbuffer(xxbuffer,yybuffer,zzbuffer,nx,ny);
  rayimage normalbuffer(nxbuffer,nybuffer,nzbuffer,nx,ny);
  
  //Initialize zbuffer
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;
  std::fill(zbuffer_depth.begin(), zbuffer_depth.end(), std::numeric_limits<float>::infinity() ) ;
  
  //Initialize Shadow Map bounds and orientation
  //If changed to 0.1-100.0 doesn't work anymore
  float near_plane = 1.0f, far_plane = 10.0f;
  vec3 light_up = vec3(0.,1.,0.);
  if(glm::length(glm::cross(light_up,light_dir)) == 0) {
    light_up = vec3(0.f,0.f,1.0f);
  }
  
  vec3 sceneboundmin = vec3(bounds(0),bounds(1),bounds(2));
  vec3 sceneboundmax = vec3(bounds(3),bounds(4),bounds(5));
  float scene_diag = glm::length(sceneboundmax-sceneboundmin);
  vec3 scene_center = (sceneboundmax+sceneboundmin)/2.0f;
  
  glm::mat4 lightProjection = glm::ortho(-scene_diag/2, scene_diag/2, -scene_diag/2, scene_diag/2, 
                                         near_plane, far_plane);
  glm::mat4 lightView = glm::lookAt(scene_center+light_dir,
                                    scene_center,
                                    light_up);

  Mat M = vp_shadow*lightProjection*lightView*Model;
  Mat uniform_Mshadow_ = M * glm::inverse(vp * Projection * View * Model);
  
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
    float shininess = !override_exponent ? as<float>(single_material["shininess"]) : exponent;
    float ior = as<float>(single_material["ior"]);
    float dissolve = as<float>(single_material["dissolve"]);
    float illum = as<float>(single_material["illum"]);
    String ambient_texname = as<String>(single_material["ambient_texname"]);
    String diffuse_texname = as<String>(single_material["diffuse_texname"]);
    String specular_texname = as<String>(single_material["specular_texname"]);
    String normal_texname = as<String>(single_material["normal_texname"]);
    String emissive_texname = as<String>(single_material["emissive_texname"]);
    // bool has_norms = has_normals_vec(i);
    // bool has_tex = has_tex_vec(i);
    
    bool has_texture_single          = has_texture(i);
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
      emission_intensity,
      diffuse_intensity,
      specular_intensity,
      true, //THIS SHOULD BE FIXED, but isn't currently used -- has_norms
      true, //THIS SHOULD BE FIXED, but isn't currently used -- has_tex
      has_texture_single,
      has_normal_texture_single,
      has_specular_texture_single,
      has_emissive_texture_single
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
  
  //Initialize default material
  Rcpp::String fill("");
  material_info default_mat = {
    vec3(ambient_color(0),ambient_color(1),ambient_color(2)),
    color,
    vec3(1.0),
    vec3(0.0),
    vec3(0.0),
    exponent,
    1.0,
    0.0,
    0.0,
    fill,fill,fill,fill,fill,
    max_indices,              //Maybe an issue?
    emission_intensity,
    diffuse_intensity,
    specular_intensity,
    true, //THIS SHOULD BE FIXED, but isn't currently used -- has_norms
    true, //THIS SHOULD BE FIXED, but isn't currently used -- has_tex
    false,
    false,
    false,
    false
  };
  
  //Add default shader to vector
  if(typevals(0) == 1) {
    shaders.push_back(new GouraudShader(Model, Projection, View, viewport,
                               glm::normalize(light_dir), 
                               shadowbuffer, uniform_Mshadow_, has_shadow_map,
                               shadow_map_bias,default_mat));
  } else if (typevals(0) == 2 || typevals(0) == 4 || typevals(0) == 5) {
    shaders.push_back(new DiffuseShader(Model, Projection, View, viewport,
                               glm::normalize(light_dir), 
                               shadowbuffer, uniform_Mshadow_, has_shadow_map,
                               shadow_map_bias,default_mat));
  } else if (typevals(0) == 3 || typevals(0) == 6 || typevals(0) == 7) {
    shaders.push_back(new PhongShader(Model, Projection, View, viewport,
                             glm::normalize(light_dir), 
                             shadowbuffer, uniform_Mshadow_, has_shadow_map,
                             shadow_map_bias,default_mat));
  }
  
  
  //Initialize Model vectors
  List shapes = as<List>(mesh["shapes"]);
  int number_shapes = shapes.size();
  std::vector<ModelInfo> models;

  //Initialize vertex storage vectors
  std::vector<std::vector<std::vector<vec4>  > > ndc_verts;
  std::vector<std::vector<std::vector<float> > > ndc_inv_w;
  std::vector<std::vector<std::vector<vec4>  > > ndc_verts_depth;
  std::vector<std::vector<std::vector<float> > > ndc_inv_w_depth;
  
  //Fill vectors for each shape in the model
  //order: [model_num][triangle vertex][face]
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
    if(has_shadow_map) {
      ndc_verts_depth.push_back(std::vector<std::vector<vec4>  >(3, std::vector<vec4>(n)));
      ndc_inv_w_depth.push_back(std::vector<std::vector<float> >(3, std::vector<float>(n)));
    }
    
    //Create model object
    ModelInfo model(shape_verts, shape_inds, shape_texcoords, shape_normals, shape_materials,
                    has_normals_vec(i), has_tex_vec(i), tbn);
    models.push_back(model);
  }

  //Set up blocks
  int blocksize = block_size;
  
  //Inner-most index model, then block, then index
  std::vector<std::vector<std::vector<int> > > blocks;
  
  std::vector<vec2> min_block_bound;
  std::vector<vec2> max_block_bound;
  int nx_blocks = ceil((float)nx/(float)blocksize);
  int ny_blocks = ceil((float)ny/(float)blocksize);
  
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
      for(int k = 0; k < models.size(); k++) {
        std::vector<int> model_inds_temp;
        //This is a vector for per-model indices in that specific block
        //blocks[j + ny_blocks * i][model_num]
        blocks[j + ny_blocks * i].push_back(model_inds_temp);
      }
    }
  }
  
  //Inner-most index model, then block, then index
  std::vector<std::vector<std::vector<int> > > blocks_depth;
  
  std::vector<vec2> min_block_bound_depth;
  std::vector<vec2> max_block_bound_depth;
  int nx_blocks_depth = ceil((float)nx_d/(float)blocksize);
  int ny_blocks_depth = ceil((float)ny_d/(float)blocksize);
  
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
      for(int k = 0; k < models.size(); k++) {
        std::vector<int> model_inds_temp;
        //This is a vector for per-model indices in that specific block
        //blocks[j + ny_blocks * i][model_num]
        blocks_depth[j + ny_blocks_depth * i].push_back(model_inds_temp);
      }
    }
  }
  
  std::vector<IShader*> depthshaders;
  for(int i = 0; i < number_materials+1; i++ ) {
    depthshaders.push_back(new DepthShader(Model, lightProjection, 
                                           lightView, viewport_depth, light_dir, 
                                           max_indices));
  }

  if(has_shadow_map) {
    for(int model_num = 0; model_num < models.size(); model_num++ ) {
      ModelInfo &shp = models[model_num];
      for(int i = 0; i < shp.num_indices; i++) {
        int mat_num = shp.materials[i] >= 0 && shp.materials[i] < shaders.size() ? 
          shp.materials[i] : shaders.size()-1;
        
        ndc_verts_depth[model_num][0][i] = depthshaders[mat_num]->vertex(i,0, shp);
        ndc_verts_depth[model_num][1][i] = depthshaders[mat_num]->vertex(i,1, shp);
        ndc_verts_depth[model_num][2][i] = depthshaders[mat_num]->vertex(i,2, shp);
        
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
        
        int min_x_block = std::fmax(floor(min_bounds.x / (float)blocksize), 0);
        int min_y_block = std::fmax(floor(min_bounds.y / (float)blocksize), 0);
        int max_x_block = std::fmin(ceil(max_bounds.x  / (float)blocksize), nx_blocks_depth);
        int max_y_block = std::fmin(ceil(max_bounds.y  / (float)blocksize), ny_blocks_depth);
        if(max_x_block >= 0 && max_y_block >= 0 && min_x_block < nx_blocks_depth && min_y_block < ny_blocks_depth) {
          for(int j = min_x_block; j < max_x_block; j++) {
            for(int k = min_y_block; k < max_y_block; k++) {
              blocks_depth[k + ny_blocks_depth * j][model_num].push_back(i); 
            }
          }
        }
      }
    }
    
    //Calculate shadow buffer
    auto task = [&depthshaders, &blocks_depth, &ndc_verts_depth, &ndc_inv_w_depth,  
                 &min_block_bound_depth, &max_block_bound_depth,
                 &zbuffer_depth, &shadowbuffer, &normalbuffer, &positionbuffer, &models] (unsigned int i) {
      fill_tri_blocks(blocks_depth[i],
                      ndc_verts_depth,
                      ndc_inv_w_depth,
                      min_block_bound_depth[i],
                      max_block_bound_depth[i],
                      depthshaders,
                      zbuffer_depth,
                      shadowbuffer,
                      normalbuffer,
                      positionbuffer,
                      models, true);
    };
    
    RcppThread::ThreadPool pool2(numbercores);
    for(int i = 0; i < nx_blocks_depth*ny_blocks_depth; i++) {
      pool2.push(task, i);
    }
    pool2.join();
  }

  //Calculate Image
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;

  for(int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(int i = 0; i < shp.num_indices; i++) {

      int mat_num = shp.materials[i] >= 0 && shp.materials[i] < shaders.size() ?
        shp.materials[i] : shaders.size()-1;
      ndc_verts[model_num][0][i] = shaders[mat_num]->vertex(i,0, shp);
      ndc_verts[model_num][1][i] = shaders[mat_num]->vertex(i,1, shp);
      ndc_verts[model_num][2][i] = shaders[mat_num]->vertex(i,2, shp);
    
      //Depth clamping
      ndc_verts[model_num][0][i].w = ndc_verts[model_num][0][i].w < near_clip ? near_clip : ndc_verts[model_num][0][i].w;
      ndc_verts[model_num][1][i].w = ndc_verts[model_num][1][i].w < near_clip ? near_clip : ndc_verts[model_num][1][i].w;
      ndc_verts[model_num][2][i].w = ndc_verts[model_num][2][i].w < near_clip ? near_clip : ndc_verts[model_num][2][i].w;

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
      
      int min_x_block = std::fmax(floor(min_bounds.x / (float)blocksize), 0);
      int min_y_block = std::fmax(floor(min_bounds.y / (float)blocksize), 0);
      int max_x_block = std::fmin(ceil(max_bounds.x  / (float)blocksize), nx_blocks);
      int max_y_block = std::fmin(ceil(max_bounds.y  / (float)blocksize), ny_blocks);
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
               &zbuffer, &image, &normalbuffer, &positionbuffer] (unsigned int i) {
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
                    models, false);
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
  
  for(int i = 0; i < depthshaders.size(); i++) {
    delete depthshaders[i];
  }
  for(int i = 0; i < shaders.size(); i++) {
    delete shaders[i];
  }

  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b, _["amb"] = abuffer, _["depth"] = zbuffer,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer));
}

#endif