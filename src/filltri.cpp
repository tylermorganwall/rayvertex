#include "filltri.h"

// static void print_vec(vec3 m) {
//   RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << "\n";
// }
// 
// static void print_vec(glm::dvec4 m) {
//   RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << "\n";
// }

inline Float DifferenceOfProducts(Float a, Float b, Float c, Float d) {
  Float cd = c * d;
  Float err = std::fma(-c, d, cd);
  Float dop = std::fma(a, b, -cd);
  return(dop + err);
}

inline Float edgeFunction(const vec3 &a, const vec3 &b, const vec3 &c) {
  return(DifferenceOfProducts((c.x - a.x),(b.y - a.y),(c.y - a.y),(b.x - a.x)));
}

void fill_tri_blocks(std::vector<std::vector<int> >&  block_faces,
                     std::vector<std::vector<std::vector<vec4> >  >& ndc_verts,
                     std::vector<std::vector<std::vector<Float> > >& ndc_inv_w,
                     vec2 min_block_bound,
                     vec2 max_block_bound,
                     std::vector<IShader*> shaders,
                     Rcpp::NumericMatrix &zbuffer, 
                     rayimage& image, 
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     rayimage& uv_buffer,
                     std::vector<ModelInfo> &models,
                     bool depth, 
                     std::vector<std::map<Float, alpha_info> >& alpha_depths) {
  unsigned int ny = image.height();
  
  for(unsigned int model_num = 0; model_num < models.size(); model_num++ ) {
    ModelInfo &shp = models[model_num];
    for(unsigned int entry=0; entry < block_faces[model_num].size(); entry++) {
      int face = block_faces[model_num][entry];
      
      vec3 v1 = ndc_verts[model_num][0][face] * ndc_inv_w[model_num][0][face];
      vec3 v2 = ndc_verts[model_num][1][face] * ndc_inv_w[model_num][1][face];
      vec3 v3 = ndc_verts[model_num][2][face] * ndc_inv_w[model_num][2][face];
      
      Float v1_ndc_inv_w = ndc_inv_w[model_num][0][face];
      Float v2_ndc_inv_w = ndc_inv_w[model_num][1][face];
      Float v3_ndc_inv_w = ndc_inv_w[model_num][2][face];

      int mat_num = shp.materials[face] >= 0 && shp.materials[face] < (int)shaders.size() ? 
        shp.materials[face] : shaders.size()-1;
      
      int culling = shaders[mat_num]->get_culling();
    
      bool not_culled = culling == 1 ? cross(v2-v1, v3-v2).z > 0 :
        culling == 2 ? cross(v2-v1, v3-v2).z < 0 : true;
      not_culled = !depth ? not_culled : true;
      
      if(not_culled) {
        vec3 bound_min = vec3(fmin(v1.x,fmin(v2.x,v3.x)),
                              fmin(v1.y,fmin(v2.y,v3.y)),
                              fmin(v1.z,fmin(v2.z,v3.z)));
        vec3 bound_max = vec3(fmax(v1.x,fmax(v2.x,v3.x)),
                              fmax(v1.y,fmax(v2.y,v3.y)),
                              fmax(v1.z,fmax(v2.z,v3.z)));
        
        
        unsigned int xmin =  std::min(std::max((int)floor(bound_min.x),(int)min_block_bound.x ),(int)min_block_bound.x);
        unsigned int xmax =  std::max(std::min((int)ceil(bound_max.x), (int)max_block_bound.x), (int)max_block_bound.x);
        unsigned int ymin =  std::min(std::max((int)floor(bound_min.y),(int)min_block_bound.y), (int)min_block_bound.y);
        unsigned int ymax =  std::max(std::min((int)ceil(bound_max.y), (int)max_block_bound.y ),(int)max_block_bound.y);
        
        Float area =  edgeFunction(v3, v2, v1); 
        Float inv_area = 1.0f/area;
        
        vec4 color;
        vec3 position;
        vec3 normal;
        
        Float p_step_32 = -(v2.x-v3.x);
        Float p_step_13 = -(v3.x-v1.x);
        Float p_step_21 = -(v1.x-v2.x);
        
        Float pi_step_32 = (v2.y-v3.y);
        Float pi_step_13 = (v3.y-v1.y);
        Float pi_step_21 = (v1.y-v2.y);
        
        vec3 p  = vec3((Float)xmin + 0.5f, (Float)ymin + 0.5f, 0.0f);
        
        Float w1_init = edgeFunction(v3, v2, p);
        Float w2_init = edgeFunction(v1, v3, p);
        Float w3_init = edgeFunction(v2, v1, p);
        
        //This updates w1_p and w1 from their base value--repeated addition results in
        //tearing of polygons due to loss of precision.
        for (uint32_t i = xmin; i < xmax; i++) {
          Float w1_p = w1_init + (i-xmin) * pi_step_32;
          Float w2_p = w2_init + (i-xmin) * pi_step_13;
          Float w3_p = w3_init + (i-xmin) * pi_step_21;
          for (uint32_t j = ymin; j < ymax; j++) {
            Float w1 = w1_p + (j-ymin) * p_step_32;
            Float w2 = w2_p + (j-ymin) * p_step_13;
            Float w3 = w3_p + (j-ymin) * p_step_21;
            
            bool inside = culling == 1 ? (w1 >= 0 && w2 >= 0 && w3 >= 0) : 
              culling == 2 ? (w1 <= 0 && w2 <= 0 && w3 <= 0) :
              (w1 >= 0 && w2 >= 0 && w3 >= 0) || (w1 <= 0 && w2 <= 0 && w3 <= 0);
            if (inside) {
              vec3 bc       = vec3(w1, w2, w3)*inv_area;
              vec3 bc_clip  = vec3(bc.x*v1_ndc_inv_w, bc.y*v2_ndc_inv_w, bc.z*v3_ndc_inv_w);
              bc_clip      /= (bc_clip.x + bc_clip.y + bc_clip.z);
              
              //Using bc_clip results in wrong zbuffer values here--bug?
              // Float z = v1.z * bc_clip.x + v2.z * bc_clip.y + v3.z * bc_clip.z;
              Float z = v1.z * bc.x + v2.z * bc.y + v3.z * bc.z;
              if(z > zbuffer(i,j)) continue;
              
              bool discard = shaders[mat_num]->fragment(bc_clip, color, position, normal, face);
              bool is_translucent = shaders[mat_num]->is_translucent();
              if(!discard) {
                if (depth) {
                  if(color.w >= 1.0f) {
                    zbuffer(i,j) = z;
                    image.set_color(i,j,vec3(position));
                  } else {
                    alpha_info tmp_data;
                    if(is_translucent) {
                      tmp_data.color = color;
                    } else {
                      tmp_data.color = vec4(0.0,0.0,0.0,color.w);
                    }
                    tmp_data.normal = normal;
                    tmp_data.position = position;
                    tmp_data.uv = bc_clip;
                    alpha_depths[j + ny*i][z] = tmp_data;
                  }
                } else {
                  if(color.w >= 1.0f) {
                    zbuffer(i,j) = z;
                    image.set_color(i,j,vec3(color));
                    normal_buffer.set_color(i,j,normal);
                    position_buffer.set_color(i,j,position);
                    uv_buffer.set_color(i,j,bc_clip);
                  } else {
                    alpha_info tmp_data;
                    tmp_data.color = color;
                    tmp_data.normal = normal;
                    tmp_data.position = position;
                    tmp_data.uv = bc_clip;
                    alpha_depths[j + ny*i][z] = tmp_data;
                  }
                }
              }
            } 
          }
        }
      }
    }
  }
}