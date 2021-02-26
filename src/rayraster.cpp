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
  
  vec3 color2;
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
          shader->fragment(bc,color2);
          image.set_color(i,j,color2);
        }
      } 
    }
  }
}

// [[Rcpp::export]]
List rasterize(NumericMatrix verts, IntegerMatrix inds, 
               NumericMatrix texcoords, NumericMatrix normals,
               int nx, int ny,
               CharacterVector texture_location,     
               CharacterVector normal_texture_location, 
               CharacterVector specular_texture_location,
               NumericVector lookfrom,
               NumericVector lookat,
               float fov,
               NumericVector light_direction,
               NumericVector ambient_color, 
               float exponent, float specular_intensity, float diffuse_intensity, 
               float near_clip = 0.1,
               float  far_clip = 100) {
  vec3    eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));  //lookat
  
  glm::mat4 View     = glm::lookAt(eye, center, vec3(0.,1.,0.));
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
  
  rayimage image(r,g,b,nx,ny);
  
  NumericMatrix zbuffer(nx,ny);
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<float>::infinity() ) ;

  vec3 ambient(ambient_color(0),ambient_color(1),ambient_color(2)); 
  
  float* texture;
  float* normal_texture;
  float* specular_texture;
  
  int nx_t = 0;
  int ny_t = 0;
  int nn_t = 0;
  int nx_nt = 0;
  int ny_nt = 0;
  int nn_nt = 0;
  int nx_st = 0;
  int ny_st = 0;
  int nn_st = 0;
  texture = stbi_loadf(texture_location(0), &nx_t, &ny_t, &nn_t, 4);
  normal_texture = stbi_loadf(normal_texture_location(0), &nx_nt, &ny_nt, &nn_nt, 4);
  specular_texture = stbi_loadf(specular_texture_location(0), 
                                &nx_st, &ny_st, &nn_st, 4);
  
  if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
    throw std::runtime_error("image loading failed");
  }
  
  ModelInfo model(verts, inds, texcoords, normals,
                  texture, normal_texture, specular_texture, 
                  ambient, exponent, specular_intensity, diffuse_intensity,
                  nx_t, ny_t, nn_t,
                  nx_nt, ny_nt, nn_nt);
  
  int n = inds.nrow();
  int cols = inds.ncol();
  if(cols < 2) {
    throw std::runtime_error("Too few columns in index matrix");
  }
  
  vec3 light_dir(light_direction(0),light_direction(1),light_direction(2));
  light_dir = glm::normalize(light_dir);
  
  std::unique_ptr<IShader> shader(new PhongShader(Model, Projection, View, viewport,
                                            light_dir, model));
  
  for(int i = 0; i < n; i++) {
    fill_tri(i, shader.get(), zbuffer, image);
  } 
  
  stbi_image_free(texture);
  stbi_image_free(normal_texture);
  stbi_image_free(specular_texture);
  
  
  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b));
}

#endif