#ifndef RAYVERTEX_SCREEN_VIEW_H
#define RAYVERTEX_SCREEN_VIEW_H
#include <cstddef>
#include "Rcpp.h"

// Construct on the R thread after allocation. Worker access is plain pointer
// arithmetic; the owning R matrix remains rooted until the pass barrier.
struct ScreenMatrixView {
  double* data;
  std::size_t stride;
  explicit ScreenMatrixView(Rcpp::NumericMatrix& matrix)
    : data(matrix.begin()), stride(matrix.nrow()) {}
  double& operator()(int x, int y) const {
    return data[static_cast<std::size_t>(x) + stride * y];
  }
};
#endif
