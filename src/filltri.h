#ifndef FILLTRIH
#define FILLTRIH

#include "glm.hpp"
#include "Rcpp.h"
#include "shaders.h"
#include "RcppThread.h"
#include "fragment_arena.h"
#include "defines.h"
#include "raster_profile.h"
#include "triangle_setup.h"

using namespace Rcpp;

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
                     RasterCounters* counters = nullptr, bool visibility = false,
                     bool block_coverage = false);


#endif
