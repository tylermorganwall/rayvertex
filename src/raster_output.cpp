#include <Rcpp.h>
#include <cmath>
#include <limits>

// Preserve rayimage's sRGB decoding arithmetic, including replacing NA/NaN
// before decoding. Alpha is cleaned but never decoded or clamped.
static double raster_decode(double value) {
  if (std::isnan(value)) return 0.0;
  return value <= 0.04045 ? value / 12.92 :
    std::pow((value + 0.055) / 1.055, 2.4);
}

// [[Rcpp::export]]
Rcpp::NumericVector assemble_raster_output(Rcpp::NumericMatrix red,
                                         Rcpp::NumericMatrix green,
                                         Rcpp::NumericMatrix blue,
                                         Rcpp::NumericMatrix alpha) {
  const int width=red.nrow(), height=red.ncol();
  if (green.nrow()!=width || green.ncol()!=height ||
      blue.nrow()!=width || blue.ncol()!=height ||
      alpha.nrow()!=width || alpha.ncol()!=height)
    Rcpp::stop("Raster output channels must have identical dimensions");
  const R_xlen_t samples=red.size();
  if (samples>R_XLEN_T_MAX/4) Rcpp::stop("Raster output is too large");
  Rcpp::NumericVector result(Rcpp::no_init(samples*4));
  result.attr("dim")=Rcpp::IntegerVector::create(height,width,4);
  const double* channels[]={red.begin(),green.begin(),blue.begin(),alpha.begin()};
  double* output=result.begin();
  // Write contiguous destination columns, composing the existing flipx then
  // transpose with planar assembly. Inputs are read-only and remain rooted.
  for (int x=0;x<width;++x) {
    if (x%64==0) Rcpp::checkUserInterrupt();
    for (int y=0;y<height;++y) {
      const R_xlen_t source=x+static_cast<R_xlen_t>(width)*y;
      const R_xlen_t target=height-1-y+static_cast<R_xlen_t>(height)*x;
      for (int channel=0;channel<3;++channel)
        output[target+samples*channel]=raster_decode(channels[channel][source]);
      const double a=channels[3][source];
      output[target+samples*3]=std::isnan(a) ? 0.0 : a;
    }
  }
  return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector clamp_raster_output(Rcpp::NumericVector image) {
  Rcpp::IntegerVector dims=image.attr("dim");
  if (dims.size()!=3 || dims[2]!=4)
    Rcpp::stop("Raster output must be an RGBA array");
  // Never mutate an R alias or an image retained by a caller/test. clone also
  // preserves rayimage/color-space metadata; only RGB sample values change.
  Rcpp::NumericVector result=Rcpp::clone(image);
  double* pixels=result.begin();
  const R_xlen_t rgb_samples=result.size()/4*3;
  for (R_xlen_t i=0;i<rgb_samples;++i) {
    if (i%1048576==0) Rcpp::checkUserInterrupt();
    if (pixels[i]<0.0) pixels[i]=0.0;
    else if (pixels[i]>1.0) pixels[i]=1.0;
  }
  return result;
}
