#include "raster_utils.h"
#include "filltri.h"

template<bool Collect>
void fill_tri_blocks_impl(const TriangleBins& bins, std::size_t tile,
                     const std::vector<IShader*>& shaders,
                     Rcpp::NumericMatrix &zbuffer, 
                     rayimage& image, 
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     rayimage& uv_buffer,
                     bool depth, 
                     FragmentArena& alpha_depths,
                     Rcpp::IntegerMatrix* material_id_buffer,
                     RasterCounters* counters) {
  bool write_material_ids = (!depth && material_id_buffer != nullptr);
  const auto min_block_bound=bins.minimum(tile), max_block_bound=bins.maximum(tile);
  for(std::size_t entry=bins.begin(tile);entry<bins.end(tile);++entry) {
    const auto& setup=bins.at(entry);
    const auto& v1=setup.vertices[0]; const auto& v2=setup.vertices[1]; const auto& v3=setup.vertices[2];
    const int mat_num=setup.material, global_face=setup.face, culling=setup.culling;
    const Float v1_ndc_inv_w=setup.inverse_w.x, v2_ndc_inv_w=setup.inverse_w.y, v3_ndc_inv_w=setup.inverse_w.z;
    const unsigned int xmin=std::min(std::max(setup.xmin,int(min_block_bound.x)),int(max_block_bound.x));
    const unsigned int xmax=std::max(std::min(setup.xmax,int(max_block_bound.x)),int(min_block_bound.x));
    const unsigned int ymin=std::min(std::max(setup.ymin,int(min_block_bound.y)),int(max_block_bound.y));
    const unsigned int ymax=std::max(std::min(setup.ymax,int(max_block_bound.y)),int(min_block_bound.y));
    const Float inv_area=setup.inverse_area;
    const Float p_step_32=setup.step_y.x, p_step_13=setup.step_y.y, p_step_21=setup.step_y.z;
    const Float pi_step_32=setup.step_x.x, pi_step_13=setup.step_x.y, pi_step_21=setup.step_x.z;
    vec4 color;
    vec3 position, normal;
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
            
            if constexpr (Collect) ++counters->candidates;
            bool inside = culling == 1 ? (w1 >= 0 && w2 >= 0 && w3 >= 0) : 
                          culling == 2 ? (w1 <= 0 && w2 <= 0 && w3 <= 0) :
                          ((w1 >= 0 && w2 >= 0 && w3 >= 0) ||
                           (w1 <= 0 && w2 <= 0 && w3 <= 0));
            if (inside) {
              if constexpr (Collect) ++counters->covered;
              vec3 bc       = vec3(w1, w2, w3)*inv_area;
              // Screen depth is affine; reject before perspective normalization.
              // Keep equality eligible so the later submitted fragment wins.
              Float z = v1.z * bc.x + v2.z * bc.y + v3.z * bc.z;
              if(z > zbuffer(i,j)) {
                if constexpr (Collect) ++counters->early_z;
                continue;
              }
              vec3 bc_clip = vec3(bc.x*v1_ndc_inv_w,
                                  bc.y*v2_ndc_inv_w,
                                  bc.z*v3_ndc_inv_w);
              bc_clip /= (bc_clip.x + bc_clip.y + bc_clip.z);

              if constexpr (Collect) ++counters->shaded;
              bool discard = shaders[mat_num]->fragment(bc_clip, color, position, normal, global_face);
              bool is_translucent = shaders[mat_num]->is_translucent();
              if(!discard) {
                if constexpr (Collect) { if(color.w < 1.0f) ++counters->transparent; }
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
                    alpha_depths.insert(i, j, z, tmp_data);
                  }
                } else {
                  // Main color pass.
                  if(color.w >= 1.0f) {
                    // Opaque (or effectively opaque): commit as topmost
                    zbuffer(i,j) = z;
                    image.set_color(i,j,vec4(color));
                    normal_buffer.set_color(i,j,normal);
                    position_buffer.set_color(i,j,position);
                    uv_buffer.set_color(i,j,bc_clip);

					if(write_material_ids) {
						(*material_id_buffer)(i, j) = mat_num;
					}
                  } else {
                    // Translucent: store for later compositing.
                    alpha_info tmp_data;
                    tmp_data.color = color;
                    tmp_data.normal = normal;
                    tmp_data.position = position;
                    tmp_data.uv = bc_clip;
                    alpha_depths.insert(i, j, z, tmp_data);
                    // Note: if we want correct material IDs after blending
                    // translucent layers, we'll also need material_id in
                    // alpha_info and update material_id_buffer in the
                    // final resolve loop.
                  }
                }
              }
            } 
          }
        }
  }
}

void fill_tri_blocks(const TriangleBins& bins, std::size_t tile,
                     const std::vector<IShader*>& shaders,
                     Rcpp::NumericMatrix &zbuffer,
                     rayimage& image,
                     rayimage& normal_buffer,
                     rayimage& position_buffer,
                     rayimage& uv_buffer,
                     bool depth,
                     FragmentArena& alpha_depths,
                     Rcpp::IntegerMatrix* material_id_buffer,
                     RasterCounters* counters) {
  if(counters) fill_tri_blocks_impl<true>(bins, tile, shaders, zbuffer, image, normal_buffer, position_buffer, uv_buffer, depth, alpha_depths, material_id_buffer, counters);
  else fill_tri_blocks_impl<false>(bins, tile, shaders, zbuffer, image, normal_buffer, position_buffer, uv_buffer, depth, alpha_depths, material_id_buffer, counters);
}
