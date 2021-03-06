#include "tonemap.h"
#include "Rcpp.h"
using namespace Rcpp;


static inline float reinhard(float color, float sum) {
  color = color*sum/(1 + sum);
  return(std::pow(color,1/2.2));
}


static float uncharted(float x) {
  return(((x*(A*x+C*B)+D*E)/(x*(A*x+B)+D*F))-E/F);
}

static float hable(float color) {
  float exposure_bias = 2.0f;
  float curr = uncharted(exposure_bias*color);
  float whiteScale = 1.0f/uncharted(W);
  color = curr*whiteScale;
  return(std::pow(color,1/2.2));
}

static float hbd(float color) {
  float x = color-0.004 > 0 ? color - 0.004 : 0;
  float retcolor = (x*(6.2*x+.5))/(x*(6.2*x+1.7)+0.06);
  return(retcolor);
}

Rcpp::List tonemap_image(Rcpp::NumericMatrix routput, Rcpp::NumericMatrix goutput, Rcpp::NumericMatrix boutput, 
                         int toneval) {
  int ny = routput.ncol();
  int nx = routput.nrow();
  
  for(int j = ny - 1; j >= 0; j--) {
    for(int i = 0; i < nx; i++) {
      if(toneval == 1) {
        routput(i,j) = std::pow(routput(i,j),1.0f/2.2f);
        goutput(i,j) = std::pow(goutput(i,j),1.0f/2.2f);
        boutput(i,j) = std::pow(boutput(i,j),1.0f/2.2f);
      } else if (toneval == 2) {
        float max = (routput(i,j)+goutput(i,j)+boutput(i,j))/3.0f;
        routput(i,j) = reinhard(routput(i,j),max);
        goutput(i,j) = reinhard(goutput(i,j),max);
        boutput(i,j) = reinhard(boutput(i,j),max);
      } else if (toneval == 3) {
        routput(i,j) = hable(routput(i,j));
        goutput(i,j) = hable(goutput(i,j));
        boutput(i,j) = hable(boutput(i,j));
      } else if (toneval == 4) {
        routput(i,j) = hbd(routput(i,j));
        goutput(i,j) = hbd(goutput(i,j));
        boutput(i,j) = hbd(boutput(i,j));
      } else {
        //do nothing
      }
    }
  }
  return(Rcpp::List::create(_["r"] = routput, _["g"] = goutput, _["b"] = boutput));
}