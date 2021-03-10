#ifndef FILLTRIH
#define FILLTRIH

#include "glm.hpp"
#include "Rcpp.h"
#include "shaders.h"
#include "RcppThread.h"


static void print_vec(vec3 m) {
  // RcppThread::Rcout.precision(5);
  RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << "\n";
}

inline float DifferenceOfProducts(float a, float b, float c, float d) {
  float cd = c * d;
  float err = std::fma(-c, d, cd);
  float dop = std::fma(a, b, -cd);
  return(dop + err);
}

float edgeFunction(const vec3 &a, const vec3 &b, const vec3 &c) { 
  return(DifferenceOfProducts((c.x - a.x),(b.y - a.y),(c.y - a.y),(b.x - a.x)));
} 

void fill_tri_blocks(std::vector<std::vector<std::vector<int> > >  block_faces,
                     std::vector<std::vector<std::vector<vec4> >  >& ndc_verts,
                     std::vector<std::vector<std::vector<float> > >& ndc_inv_w,
                     vec2 min_block_bound,
                     vec2 max_block_bound,
                     std::vector<IShader*> shaders,
                     Rcpp::NumericMatrix &zbuffer, 
                     rayimage& image, 
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     std::vector<ModelInfo> &models,
                     int block_i) { 
  for(int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(int entry=0; entry < block_faces[model_num][block_i].size(); entry++) {
      int face = block_faces[model_num][block_i][entry];
      
      vec3 v1 = ndc_verts[model_num][0][face] * ndc_inv_w[model_num][0][face];
      vec3 v2 = ndc_verts[model_num][1][face] * ndc_inv_w[model_num][1][face];
      vec3 v3 = ndc_verts[model_num][2][face] * ndc_inv_w[model_num][2][face];
      
      float v1_ndc_inv_w = ndc_inv_w[model_num][0][face];
      float v2_ndc_inv_w = ndc_inv_w[model_num][1][face];
      float v3_ndc_inv_w = ndc_inv_w[model_num][2][face];
      
      //Backface culling
      if(cross(v2-v1, v3-v2).z > 0) {
        vec3 bound_min = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                              fmin(v1.y,fmin(v2.y,v3.y)),
                              fmin(v1.z,fmin(v2.z,v3.z)));
        vec3 bound_max = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                              fmax(v1.y,fmax(v2.y,v3.y)),
                              fmax(v1.z,fmax(v2.z,v3.z)));
        
        int xmin =  std::min(std::max((int)floor(bound_min.x),(int)min_block_bound.x ),(int)min_block_bound.x);
        int xmax =  std::max(std::min((int)ceil(bound_max.x), (int)max_block_bound.x), (int)max_block_bound.x);
        int ymin =  std::min(std::max((int)floor(bound_min.y),(int)min_block_bound.y), (int)min_block_bound.y);
        int ymax =  std::max(std::min((int)ceil(bound_max.y), (int)max_block_bound.y ),(int)max_block_bound.y);
  
        float area = edgeFunction(v3, v2, v1); 
        float inv_area = 1.0f/area;
        
        vec3 color;
        vec3 position;
        vec3 normal;
        
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
        
        //Need to update w1_p and w1 in reference to their base value--repeated addition results in
        //tearing of polygons due to loss of precision.
        for (uint32_t i = xmin; i < xmax; i++) {
          float w1_p = w1_init + (i-xmin) * pi_step_32;
          float w2_p = w2_init + (i-xmin) * pi_step_13;
          float w3_p = w3_init + (i-xmin) * pi_step_21;
          for (uint32_t j = ymin; j < ymax; j++) {
            float w1 = w1_p + (j-ymin) * p_step_32;
            float w2 = w2_p + (j-ymin) * p_step_13;
            float w3 = w3_p + (j-ymin) * p_step_21;
            
            if (w1 >= 0 && w2 >= 0 && w3 >= 0) {
              vec3 bc       = vec3(w1, w2, w3)*inv_area;
              vec3 bc_clip  = vec3(bc.x*v1_ndc_inv_w, bc.y*v2_ndc_inv_w, bc.z*v3_ndc_inv_w);
              bc_clip      /= (bc_clip.x + bc_clip.y + bc_clip.z);
              
              float z = v1.z * bc_clip.x + v2.z * bc_clip.y + v3.z * bc_clip.z;
              if(z > zbuffer(i,j)) continue;
              int mat_num = shp.materials[i] != -1 ? shp.materials[i] : shaders.size()-1;
              bool discard = shaders[mat_num]->fragment(bc_clip, color, position, normal, face);
              if(!discard) {
                zbuffer(i,j) = z;
                image.set_color(i,j,color);
                normal_buffer.set_color(i,j,normal);
                position_buffer.set_color(i,j,position);
              }
            } 
          }
        }
      }
    }
  }
}

#endif