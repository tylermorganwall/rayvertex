#ifndef WIREFRAMEH
#define WIREFRAMEH

#include "Rcpp.h"
#include "vec3.hpp"

using namespace Rcpp;

using namespace glm;

void line(int x0, int y0, int x1, int y1, NumericMatrix &r, NumericMatrix &b, NumericMatrix &g, vec3 color) { 
  bool steep = false; 
  if (std::abs(x0-x1)<std::abs(y0-y1)) { 
    std::swap(x0, y0); 
    std::swap(x1, y1); 
    steep = true; 
  } 
  if (x0>x1) { 
    std::swap(x0, x1); 
    std::swap(y0, y1); 
  } 
  int dx = x1-x0; 
  int dy = y1-y0; 
  int derror2 = std::abs(dy)*2; 
  int error2 = 0; 
  int y = y0; 
  for (int x=x0; x<=x1; x++) { 
    if (steep) { 
      r(y, x) = color[0];
      g(y, x) = color[1];
      b(y, x) = color[2];
    } else { 
      r(x, y) = color[0];
      g(x, y) = color[1];
      b(x, y) = color[2];
    } 
    error2 += derror2; 
    if (error2 > dx) { 
      y += (y1>y0?1:-1); 
      error2 -= dx*2; 
    } 
  } 
} 


void wu_line(float x0, float y0, float x1, float y1,
             NumericMatrix &r, NumericMatrix &b, NumericMatrix &g, vec3 color) {
  auto ipart = [](float x) -> int {return int(std::floor(x));};
  auto round = [](float x) -> float {return std::round(x);};
  auto fpart = [](float x) -> float {return x - std::floor(x);};
  auto rfpart = [=](float x) -> float {return 1 - fpart(x);};
  
  const bool steep = abs(y1 - y0) > abs(x1 - x0);
  if (steep) {
    std::swap(x0,y0);
    std::swap(x1,y1);
  }
  if (x0 > x1) {
    std::swap(x0,x1);
    std::swap(y0,y1);
  }
  
  const float dx = x1 - x0;
  const float dy = y1 - y0;
  const float gradient = (dx == 0) ? 1 : dy/dx;
  
  int xpx11;
  float intery;
  {
    const float xend = round(x0);
    const float yend = y0 + gradient * (xend - x0);
    const float xgap = rfpart(x0 + 0.5);
    xpx11 = int(xend);
    const int ypx11 = ipart(yend);
    if (steep) {
      r(ypx11,     xpx11) = rfpart(yend) * xgap * color[0];
      r(ypx11 + 1, xpx11) =  fpart(yend) * xgap * color[0];
      g(ypx11,     xpx11) = rfpart(yend) * xgap * color[1];
      g(ypx11 + 1, xpx11) =  fpart(yend) * xgap * color[1];
      b(ypx11,     xpx11) = rfpart(yend) * xgap * color[2];
      b(ypx11 + 1, xpx11) =  fpart(yend) * xgap * color[2];
    } else {
      r(xpx11, ypx11)     = rfpart(yend) * xgap * color[0];
      r(xpx11, ypx11 + 1) =  fpart(yend) * xgap * color[0];
      g(xpx11, ypx11)     = rfpart(yend) * xgap * color[1];
      g(xpx11, ypx11 + 1) =  fpart(yend) * xgap * color[1];
      b(xpx11, ypx11)     = rfpart(yend) * xgap * color[2];
      b(xpx11, ypx11 + 1) =  fpart(yend) * xgap * color[2];
    }
    intery = yend + gradient;
  }
  
  int xpx12;
  {
    const float xend = round(x1);
    const float yend = y1 + gradient * (xend - x1);
    const float xgap = rfpart(x1 + 0.5);
    xpx12 = int(xend);
    const int ypx12 = ipart(yend);
    if (steep) {
      if(ypx12 + 1 < r.nrow()) {
        r(ypx12,     xpx12)  = rfpart(yend) * xgap * color[0];
        r(ypx12 + 1, xpx12)  =  fpart(yend) * xgap * color[0];
        g(ypx12,     xpx12)  = rfpart(yend) * xgap * color[1];
        g(ypx12 + 1, xpx12)  =  fpart(yend) * xgap * color[1];
        b(ypx12,     xpx12)  = rfpart(yend) * xgap * color[2];
        b(ypx12 + 1, xpx12)  =  fpart(yend) * xgap * color[2];
      }
    } else {
      if(ypx12 + 1 < r.ncol()) {
        r(xpx12, ypx12)     = rfpart(yend) * xgap * color[0];
        r(xpx12, ypx12 + 1) =  fpart(yend) * xgap * color[0];
        g(xpx12, ypx12)     = rfpart(yend) * xgap * color[1];
        g(xpx12, ypx12 + 1) =  fpart(yend) * xgap * color[1];
        b(xpx12, ypx12)     = rfpart(yend) * xgap * color[2];
        b(xpx12, ypx12 + 1) =  fpart(yend) * xgap * color[2];
      }
    }
  }
  
  if (steep) {
    for (int x = xpx11 + 1; x < xpx12; x++) {
      if(ipart(intery) + 1 < r.nrow()) {
        r(ipart(intery),     x) =  rfpart(intery) * color[0];
        r(ipart(intery) + 1, x) =   fpart(intery) * color[0];
        g(ipart(intery),     x) =  rfpart(intery) * color[1];
        g(ipart(intery) + 1, x) =   fpart(intery) * color[1];
        b(ipart(intery),     x) =  rfpart(intery) * color[2];
        b(ipart(intery) + 1, x) =   fpart(intery) * color[2];
      }
      intery += gradient;
    }
  } else {
    for (int x = xpx11 + 1; x < xpx12; x++) {
      if(ipart(intery) + 1 < r.ncol()) {
        r(x, ipart(intery)    ) = rfpart(intery) * color[0];
        r(x, ipart(intery) + 1) =  fpart(intery) * color[0];
        g(x, ipart(intery)    ) = rfpart(intery) * color[1];
        g(x, ipart(intery) + 1) =  fpart(intery) * color[1];
        b(x, ipart(intery)    ) = rfpart(intery) * color[2];
        b(x, ipart(intery) + 1) =  fpart(intery) * color[2];
      }
      intery += gradient;
    }
  }
}


// [[Rcpp::export]]
List wireframe(NumericMatrix verts, IntegerMatrix inds, int nx, int ny) {
  vec3 color{1,1,1};
  
  //Initialize output matrices
  NumericMatrix r(nx,ny);
  NumericMatrix g(nx,ny);
  NumericMatrix b(nx,ny);
  
  int n = inds.nrow();
  int cols = inds.ncol();
  if(cols < 2) {
    throw std::runtime_error("Too few columns in index matrix");
  }
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < cols-1; j++) {
      float x0 = (verts(inds(i,j  ) -1 , 0) + 1)/2 * (nx-1);
      float x1 = (verts(inds(i,j+1) -1 , 0) + 1)/2 * (nx-1);
      float y0 = (verts(inds(i,j  ) -1 , 1) + 1)/2 * (ny-1);
      float y1 = (verts(inds(i,j+1) -1 , 1) + 1)/2 * (ny-1);
      
      wu_line(x0,y0,x1,y1,r,g,b,color);
    }
  } 
  
  return(List::create(_["r"] = r, _["g"] = g, _["b"] = b));
}

#endif