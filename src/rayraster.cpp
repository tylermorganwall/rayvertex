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

vec3 barycentric(vec3 *pts, vec3 P) { 
  vec3 u = cross(vec3(pts[2][0]-pts[0][0], pts[1][0]-pts[0][0], pts[0][0]-P[0]),
                 vec3(pts[2][1]-pts[0][1], pts[1][1]-pts[0][1], pts[0][1]-P[1]));
  /* `pts` and `P` has integer value as coordinates
   so `abs(u[2])` < 1 means `u[2]` is 0, that means
   triangle is degenerate, in this case return something with negative coordinates */
  if (std::fabs(u[2])>1e-2) {
    return vec3(1.0f-(u.x+u.y)/u.z, u.y/u.z, u.x/u.z); 
  }
  return vec3(-1,1,1);
} 

void fill_tri(int vertex,
              IShader* shader,
              NumericMatrix &zbuffer, 
              rayimage& image) { 
  vec3 v1 = shader->vertex(vertex,0);
  vec3 v2 = shader->vertex(vertex,1);
  vec3 v3 = shader->vertex(vertex,2);
  
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
  float area = edgeFunction(v1, v2, v3); 
// Rcpp::Rcout << xmin << " " << xmax << " " << ymin << " " << ymax << "\n";
  
  vec3 color;
  for (uint32_t i = xmin; i < xmax; i++) {
    for (uint32_t j = ymin; j < ymax; j++) {
      vec3 p((float)i + 0.5f, (float)j + 0.5f, 0.0);
      vec3 pts[3] = {v1,v2,v3};
      vec3 bc  = barycentric(pts, p);
      
      //Test if within triangle
      if (bc.x > 0 && bc.y > 0 && bc.z > 0) { 
        p.z = v1.z * bc.x + v2.z * bc.y + v3.z * bc.z;
        
        //Test depth buffer
        if(p.z < zbuffer(i,j)) {
          zbuffer(i,j) = p.z;
          shader->fragment(bc,color);
          image.set_color(i,j,color);
        }
      } 
    }
  }
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
               NumericVector model_color,
               NumericVector lookfrom,
               NumericVector lookat,
               float fov,
               NumericVector light_direction,
               NumericVector ambient_color, 
               float exponent, float specular_intensity, float diffuse_intensity, 
               int type,
               bool has_texture,
               bool has_normal_texture,
               bool has_specular_texture,
               bool has_shadow_map,
               bool tbn,
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
  NumericMatrix zbuffer(nx,ny);
  NumericMatrix sbuffer(nx,ny);
  rayimage shadowbuffer(sbuffer,sbuffer,sbuffer,nx,ny);
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;
  
  //Load textures
  float* texture;
  float* normal_texture;
  float* specular_texture;
  int nx_t, ny_t, nn_t, nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st = 0;
  
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
  
  int n = inds.nrow();
  int cols = inds.ncol();
  if(cols < 2) {
    throw std::runtime_error("Too few columns in index matrix");
  }
  
  //Create model object
  ModelInfo model(verts, inds, texcoords, normals,
                  texture, normal_texture, specular_texture, 
                  ambient, exponent, specular_intensity, diffuse_intensity,
                  nx_t, ny_t, nn_t,
                  nx_nt, ny_nt, nn_nt,
                  has_texture, has_normal_texture, has_specular_texture,
                  color, tbn);

  //Calculate Shadow Map
  float near_plane = 1.0f, far_plane = 7.5f;
  vec3 light_up = vec3(0.,1.,0.);
  if(std::fabs(glm::dot(light_up,glm::normalize(light_dir))) == 1) {
    light_up = vec3(0.f,0.f,1.0f);
  }
  //Change to bounding box scene
  
  glm::mat4 lightProjection = glm::ortho(-1.0f, 1.0f, -1.0f, 1.0f, near_plane, far_plane);
  glm::mat4 lightView = glm::lookAt(light_dir,
                                    glm::vec3( 0.0f, 0.0f,  0.0f),
                                    light_up);
  glm::mat4 lightSpaceMatrix = lightProjection * lightView;

  std::unique_ptr<IShader> depthshader(new DepthShader(Model, lightProjection, lightView, viewport,
                                                       light_dir, model));
  if(has_shadow_map) {
    for(int i = 0; i < n; i++) {
      fill_tri(i, depthshader.get(), zbuffer, shadowbuffer);
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
                                                    shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 2) {
    shader = std::unique_ptr<IShader>(new DiffuseShader(Model, Projection, View, viewport,
                                                        glm::normalize(light_dir), model,
                                                        shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 3) {
    shader = std::unique_ptr<IShader>(new PhongShader(Model, Projection, View, viewport,
                                                      glm::normalize(light_dir), model,
                                                      shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 4) {
    shader = std::unique_ptr<IShader>(new DiffuseNormalShader(Model, Projection, View, viewport,
                                                      glm::normalize(light_dir), model,
                                                      shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 5) {
    shader = std::unique_ptr<IShader>(new DiffuseShaderTangent(Model, Projection, View, viewport,
                                                              glm::normalize(light_dir), model,
                                                              shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 6) {
    shader = std::unique_ptr<IShader>(new PhongNormalShader(Model, Projection, View, viewport,
                                                            glm::normalize(light_dir), model,
                                                            shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else if (type == 7) {
    shader = std::unique_ptr<IShader>(new PhongShaderTangent(Model, Projection, View, viewport,
                                                            glm::normalize(light_dir), model,
                                                            shadowbuffer, uniform_Mshadow_, has_shadow_map));
  } else {
    throw std::runtime_error("shader not recognized");
  }
  
  for(int i = 0; i < n; i++) {
    fill_tri(i, shader.get(), zbuffer, image);
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
  
  
  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b));
}

#endif