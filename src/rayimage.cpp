#include "rayimage.h"

vec4 trivalue(float uu, float vv,  float* data, 
              int nx, int ny, int channels) {
  while(uu < 0) uu += 1;
  while(vv < 0) vv += 1;
  while(uu > 1) uu -= 1;
  while(vv > 1) vv -= 1;
  int i = uu * (float)nx;
  int j = (1-vv) * (float)ny;
  
  if (i < 0) i = 0;
  if (j < 0) j = 0;
  if (i > nx-1) i = nx-1;
  if (j > ny-1) j = ny-1;
  float r1 = data[channels*i + channels*nx*j];
  float g1 = data[channels*i + channels*nx*j+1];
  float b1 = data[channels*i + channels*nx*j+2];
  float a1 = channels == 4 ? data[channels*i + channels*nx*j+3] : 1.0;
  
  return(vec4(r1,g1,b1,a1));
}