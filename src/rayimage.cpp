#include "rayimage.h"

vec4 trivalue(Float uu, Float vv,  float* data, 
              int nx, int ny, int channels) {
  while(uu < 0) uu += 1;
  while(vv < 0) vv += 1;
  while(uu > 1) uu -= 1;
  while(vv > 1) vv -= 1;
  int i = uu * (Float)nx;
  int j = (1-vv) * (Float)ny;
  
  if (i < 0) i = 0;
  if (j < 0) j = 0;
  if (i > nx-1) i = nx-1;
  if (j > ny-1) j = ny-1;
  Float r1 = data[channels*i + channels*nx*j];
  Float g1 = data[channels*i + channels*nx*j+1];
  Float b1 = data[channels*i + channels*nx*j+2];
  Float a1 = channels == 4 ? data[channels*i + channels*nx*j+3] : 1.0;
  
  return(vec4(r1,g1,b1,a1));
}

vec4 trivalue(Float uu, Float vv, reflection_map_info ref) {
  int nx = ref.nx;
  int ny = ref.ny;
  int channels = ref.nn;
  
  while(uu < 0) uu += 1;
  while(vv < 0) vv += 1;
  while(uu > 1) uu -= 1;
  while(vv > 1) vv -= 1;
  int i = uu * (Float)nx;
  int j = (1-vv) * (Float)ny;
  
  if (i < 0) i = 0;
  if (j < 0) j = 0;
  if (i > nx-1) i = nx-1;
  if (j > ny-1) j = ny-1;
  Float r1 = ref.reflection[channels*i + channels*nx*j];
  Float g1 = ref.reflection[channels*i + channels*nx*j+1];
  Float b1 = ref.reflection[channels*i + channels*nx*j+2];

  return(vec4(r1,g1,b1,1.0));
}