#include "line.h" 

void aa_line(std::vector<vec3>& line_mat_start,
             std::vector<vec3>& line_mat_end,
             std::vector<vec3>& line_color,
             Rcpp::NumericMatrix &zbuffer,
             std::vector<std::map<Float, alpha_info> >& alpha_depths,
             Float alpha_line, Float line_offset) {
  auto ipart = [](Float x) -> int {return int(std::floor(x));};
  auto round = [](Float x) -> Float {return std::round(x);};
  auto fpart = [](Float x) -> Float {return x - std::floor(x);};
  auto rfpart = [=](Float x) -> Float {return 1 - fpart(x);};
  
  Float x0, y0, z0, x1, y1, z1;
  int nx = zbuffer.nrow();
  int ny = zbuffer.ncol();
  
  for(int ii = 0; ii < line_mat_start.size(); ii++) {
    x0 = line_mat_start[ii].x;
    x1 =   line_mat_end[ii].x;
    y0 = line_mat_start[ii].y;
    y1 =   line_mat_end[ii].y;
    //Perspective correct interpolation
    z0 = 1.0f/line_mat_start[ii].z; 
    z1 =   1.0f/line_mat_end[ii].z;
    
    const bool steep = std::fabs(y1 - y0) > std::fabs(x1 - x0);
    if (steep) {
      std::swap(x0,y0);
      std::swap(x1,y1);
    }
    if (x0 > x1) {
      std::swap(x0,x1);
      std::swap(y0,y1);
      std::swap(z0,z1);
    }
    
    const Float dx = x1 - x0;
    const Float dy = y1 - y0;
    const Float gradient = (dx == 0) ? 1 : dy/dx;
    
    Float offset = line_offset;
    //zbuffer
    Float dz = z1-z0; 
    Float zcurrent = z0;
    Float z;
    int xpx11;
    Float intery;
    {
      const Float xend = floor(x0);
      const Float yend = y0 + gradient * (xend - x0);
      const Float xgap = rfpart(x0 + 0.5);
      xpx11 = xend;
      const int ypx11 = ipart(yend);
      if (steep) {
        if(ypx11 < nx && ypx11 >= 0 && xpx11 < ny && xpx11 >= 0) {
          z = 1.0f/z0+offset;
          alpha_info tmp_data;
          // tmp_data.color = vec4(line_color[ii], rfpart(yend) * xgap* alpha_line);
          // tmp_data.color = vec4(line_color[ii], 1);
          
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          // alpha_depths[xpx11 + nx * ypx11][z] = tmp_data;

          if(xpx11 + 1 < nx) {
            alpha_info tmp_data2;
            // tmp_data2.color = vec4(line_color[ii],fpart(yend) * xgap * alpha_line);
            // tmp_data2.color = vec4(line_color[ii],1);
            
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            // alpha_depths[xpx11 + 1 + nx * ypx11][z] = tmp_data2;
          }
        }
      } else {
        if(xpx11 < nx && xpx11 >= 0 && ypx11 < ny && ypx11 >= 0) {
          z = 1.0f/z0+offset;
          alpha_info tmp_data;
          // tmp_data.color = vec4(line_color[ii], rfpart(yend) * xgap * alpha_line);
          // tmp_data.color = vec4(line_color[ii], 1);
          
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          // alpha_depths[ypx11 + ny * xpx11][z] = tmp_data;
          if(ypx11 + 1 < ny) {
            alpha_info tmp_data2;
            // tmp_data2.color = vec4(line_color[ii],fpart(yend) * xgap * alpha_line);
            // tmp_data2.color = vec4(line_color[ii],1);
            
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            // alpha_depths[(ypx11 + 1) + ny * xpx11][z] = tmp_data2;
          }
        }
      }
      intery = yend + gradient;
    }
    
    int xpx12;
    {
      const Float xend = floor(x1);
      const Float yend = y1 + gradient * (xend - x1);
      const Float xgap = rfpart(x1 + 0.5);
      xpx12 = xend;
      const int ypx12 = ipart(yend);
      if (steep) {
        if(ypx12 < nx && ypx12 >= 0 && xpx12 < ny && xpx12 >= 0) {
          z = 1.0f/z1+offset;
          alpha_info tmp_data;
          // tmp_data.color = vec4(line_color[ii], rfpart(yend) * xgap* alpha_line);
          // tmp_data.color = vec4(line_color[ii], 1);
          
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          // alpha_depths[xpx12 + nx * ypx12][z] = tmp_data;

          if(xpx12 + 1 < nx) {
            alpha_info tmp_data2;
            // tmp_data2.color = vec4(line_color[ii],fpart(yend) * xgap* alpha_line);
            // tmp_data2.color = vec4(line_color[ii],1);
            
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            // alpha_depths[xpx12 + 1 + nx * ypx12][z] = tmp_data2;
          }
        }
      } else {
        if(xpx12 < nx && xpx12 >= 0 && ypx12 < ny && ypx12 >= 0) {
          z = 1.0f/z1+offset;
          alpha_info tmp_data;
          // tmp_data.color = vec4(line_color[ii], rfpart(yend) * xgap * alpha_line);
          // tmp_data.color = vec4(line_color[ii], 1);
          
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          // alpha_depths[ypx12 + ny * xpx12][z] = tmp_data;
          if(ypx12 + 1 < ny) {
            alpha_info tmp_data2;
            // tmp_data2.color = vec4(line_color[ii],fpart(yend) * xgap * alpha_line);
            // tmp_data2.color = vec4(line_color[ii],1);
            
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            // alpha_depths[(ypx12 + 1) + ny * xpx12][z] = tmp_data2;
          }
        }
      }
    }
    Float intery0 = intery;

    int iy;
    Float zsteps = xpx12-(xpx11);
    Float zstep = dz/zsteps;
    Float counter = 0;

    if (steep) {
      for (int x = xpx11   ; x < xpx12 ; x++) {
        iy = ipart(intery)-1;
        if(iy < nx && iy >= 0 && x < ny && x >= 0) {
          z = 1.0f/zcurrent+offset;
          alpha_info tmp_data;
          tmp_data.color = vec4(line_color[ii], rfpart(intery) * alpha_line);
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          alpha_depths[x + ny * iy][z] = tmp_data;

          if(iy + 1 < nx) {
            alpha_info tmp_data2;
            tmp_data2.color = vec4(line_color[ii],fpart(intery) * alpha_line);
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            alpha_depths[x + ny * (iy+1)][z] = tmp_data2;
          }
        }
        counter++;
        zcurrent = z0 + counter * zstep;
        intery = intery0 + counter * gradient;
      }
    } else {
      for (int x = xpx11 ; x < xpx12 ; x++) {
        iy = ipart(intery)-1;
        if(x < nx && x >= 0 && iy < ny && iy >= 0) {
          z = 1.0f/zcurrent+offset;
          alpha_info tmp_data;
          tmp_data.color = vec4(line_color[ii], rfpart(intery) * alpha_line);
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          alpha_depths[iy + ny * x][z] = tmp_data;
          if(iy + 1 < ny) {
            alpha_info tmp_data2;
            tmp_data2.color = vec4(line_color[ii],fpart(intery) * alpha_line);
            tmp_data2.normal = vec3(0.);
            tmp_data2.position = vec3(0.);
            tmp_data2.uv = vec3(0.);
            alpha_depths[(iy + 1) + ny * x][z] = tmp_data2;
          }
        }
        counter++;
        zcurrent = z0 + counter * zstep;
        intery = intery0 + counter * gradient;
      }
    }
  }
}

//This takes NDC values
void noaa_line(std::vector<vec3>& line_mat_start,
               std::vector<vec3>& line_mat_end,
               std::vector<vec3>& line_color,
               Rcpp::NumericMatrix &zbuffer,
               std::vector<std::map<Float, alpha_info> >& alpha_depths,
               Float alpha_line, Float line_offset) { 
  int x0, y0, x1, y1;
  Float z0, z1;
  int nx = zbuffer.nrow();
  int ny = zbuffer.ncol();
  Float offset = line_offset;
  
  for(int ii = 0; ii < line_mat_start.size(); ii += 1) {
    x0 = line_mat_start[ii].x;
    x1 =   line_mat_end[ii].x;
    y0 = line_mat_start[ii].y;
    y1 =   line_mat_end[ii].y;
    //Perspective correct interpolation
    z0 = 1.0f/line_mat_start[ii].z; 
    z1 =   1.0f/line_mat_end[ii].z;

    bool steep = false; 
    if (std::abs(x0-x1)<std::abs(y0-y1)) { 
      std::swap(x0, y0); 
      std::swap(x1, y1); 
      
      steep = true; 
    } 
    if (x0>x1) { 
      std::swap(x0, x1); 
      std::swap(y0, y1); 
      std::swap(z0, z1); 
    } 
    int dx = x1-x0; 
    int dy = y1-y0; 
    Float dz = z1-z0; 
    Float zcurrent = z0;
  
    int derror2 = std::abs(dy)*2; 
    int error2 = 0; 
    int y = y0; 
    Float zsteps = 0;
    for (int x=x0; x<=x1; x++) { 
      error2 += derror2; 
      if (error2 > dx) { 
        y += (y1>y0?1:-1); 
        error2 -= dx*2; 
      } 
      zsteps++;
    } 
    Float zstep = dz/zsteps;
    Float z;
    derror2 = std::abs(dy)*2; 
    error2 = 0; 
    y = y0; 
    Float counter = 0;
    for (int x=x0; x<=x1; x++) { 
      if (steep) { 
        if(y < nx && y >= 0 && x < ny && x >= 0) { 
          z = 1.0f/zcurrent + offset;
          alpha_info tmp_data;
          tmp_data.color = vec4(line_color[ii],1.0f * alpha_line);
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          alpha_depths[x + nx * y][z] = tmp_data;
        }
      } else { 
        if(y < ny && y >= 0 && x < nx && x >= 0) { 
          z = 1.0f/zcurrent + offset;
          alpha_info tmp_data;
          tmp_data.color = vec4(line_color[ii],1.0f * alpha_line);
          tmp_data.normal = vec3(0.);
          tmp_data.position = vec3(0.);
          tmp_data.uv = vec3(0.);
          alpha_depths[y + ny * x][z] = tmp_data;
        }
      } 
      error2 += derror2; 
      if (error2 > dx) { 
        y += (y1>y0?1:-1); 
        error2 -= dx*2; 
      }
      counter++;
      zcurrent = z0 + counter*zstep;
    }
  }
} 
