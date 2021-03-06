#ifndef TONEMAPH
#define TONEMAPH

#include "Rcpp.h"

static inline float reinhard(float color, float sum);

static float A = 0.15;
static float B = 0.50;
static float C = 0.10;
static float D = 0.20;
static float E = 0.02;
static float F = 0.30;
static float W = 11.2;

static float uncharted(float x);
static float hable(float color);
static float hbd(float color);

// [[Rcpp::export]]
Rcpp::List tonemap_image(Rcpp::NumericMatrix routput, Rcpp::NumericMatrix goutput, Rcpp::NumericMatrix boutput, 
                         int toneval);

#endif