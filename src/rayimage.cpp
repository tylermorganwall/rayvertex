#include "rayimage.h"

vec3 trivalue(float uu, float vv,  float* data, 
              int nx, int ny, int channels) {
  int i = uu * nx;
  int j = (1-vv) * ny - 0.00001;
  if (i < 0) i = 0;
  if (j < 0) j = 0;
  if (i > nx-1) i = nx-1;
  if (j > ny-1) j = ny-1;
  float r1 = data[channels*i + channels*nx*j];
  float g1 = data[channels*i + channels*nx*j+1];
  float b1 = data[channels*i + channels*nx*j+2];
  return(vec3(r1,g1,b1));
}