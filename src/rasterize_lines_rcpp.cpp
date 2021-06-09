#define STB_IMAGE_IMPLEMENTATION 

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
#include <memory>
#include "glm.hpp"
#include "gtc/matrix_transform.hpp"
#include "defines.h"
#include "filltri.h"

#include "shaders.h"
#include "rayimage.h"

#include "line.h"

// typedef glm::dvec4 vec4;
// typedef glm::dvec3 vec3;
// typedef glm::dvec2 vec2;
// typedef glm::dmat4x4 Mat;

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
// [[Rcpp::export]]
List rasterize_lines_rcpp(NumericMatrix line_mat,
                          int nx, int ny,
                          NumericVector model_color,
                          NumericVector lookfrom,
                          NumericVector lookat,
                          double fov,
                          double near_clip,
                          double  far_clip,
                          NumericVector bounds,
                          NumericVector camera_up,
                          double alpha_line, double line_offset,
                          NumericVector ortho_dims, 
                          bool aa_lines) {
  //Convert R vectors to vec3
  vec3 eye(lookfrom(0),lookfrom(1),lookfrom(2)); //lookfrom
  vec3 center(lookat(0),lookat(1),lookat(2));    //lookat
  vec3 cam_up = vec3(camera_up(0),camera_up(1),camera_up(2));
  vec3 color(model_color(0),model_color(1),model_color(2));
  
  //Account for colinear camera direction/cam_up vectors
  if(glm::length(glm::cross(eye-center,cam_up)) == 0) {
    cam_up = vec3(0.,0.,1.f);
  }
  
  //Generate MVP matrices
  Mat View       = glm::lookAt(eye, center, cam_up);
  Mat Model      = glm::translate(Mat(1.0), vec3(0.0, 0.0, 0.0));
  Mat Projection = fov != 0.0 ? glm::perspective(glm::radians((Float)fov), 
                                                 (Float)nx / (Float)ny, 
                                                 (Float)near_clip, 
                                                 (Float)far_clip) :
    glm::ortho(-(Float)ortho_dims(0)/2, (Float)ortho_dims(0)/2, -(Float)ortho_dims(1)/2, (Float)ortho_dims(1)/2);
  vec4 viewport(0.0f, 0.0f, (Float)nx-1, (Float)ny-1);
  
  
  Mat vp = glm::scale(glm::translate(Mat(1.0f),
                                     vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)),
                                     vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  //Initialize output matrices
  NumericMatrix r(nx,ny);
  NumericMatrix g(nx,ny);
  NumericMatrix b(nx,ny);
  
  //Create buffers
  rayimage image(r,g,b,nx,ny);
  
  //Depth buffer
  NumericMatrix zbuffer(nx,ny);
  
  
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
  rayimage positionbuffer(xxbuffer,yybuffer,zzbuffer,nx,ny);
  rayimage normalbuffer(nxbuffer,nybuffer,nzbuffer,nx,ny);
  rayimage uvbuffer(uvxbuffer,uvybuffer,uvzbuffer,nx,ny);
  
  //Initialize zbuffer
  std::fill(zbuffer.begin(), zbuffer.end(), std::numeric_limits<Float>::infinity() ) ;
  
  // vec3 light_up = vec3(0.,1.,0.);
  
  // vec3 sceneboundmin = vec3(bounds(0),bounds(1),bounds(2));
  // vec3 sceneboundmax = vec3(bounds(3),bounds(4),bounds(5));
  // Float scene_diag = glm::length(sceneboundmax-sceneboundmin)+0.5;
  // vec3 scene_center = (sceneboundmax+sceneboundmin)/(Float)2.0;
  
  //For alpha transparency
  std::vector<std::map<Float, alpha_info> > alpha_depths(nx*ny);
  
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
  
  if(ndc_line_verts_start.size() > 0) {
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
  
  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b,
                      _["depth"] = zbuffer,
                      _["normalx"] = nxbuffer, _["normaly"] = nybuffer, _["normalz"] = nzbuffer,
                      _["positionx"] = xxbuffer, _["positiony"] = yybuffer, _["positionz"] = zzbuffer,
                      _["uvx"] = uvxbuffer, _["uvy"] = uvybuffer, _["uvz"] = uvzbuffer));
}

#endif
