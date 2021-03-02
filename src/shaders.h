#ifndef SHADERSH
#define SHADERSH

#include "glm.hpp"
#include "model.h"
#include "gtc/matrix_transform.hpp"
#include "gtc/matrix_access.hpp"


typedef glm::vec4 vec4;
typedef glm::vec3 vec3;
typedef glm::vec2 vec2;
typedef glm::mat4x4 Mat;

class IShader {
  public:
    virtual vec3 vertex(int iface, int nthvert) = 0;
    virtual bool fragment(vec3 bc, vec3 &color) = 0;
};

static void print_vec(vec3 m) {
  Rcpp::Rcout.precision(5);
  Rcpp::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << "\n";
}


class GouraudShader : public IShader {
  public:
    GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                  Mat uniform_Mshadow_, bool has_shadow_map);
    
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_tri[3];
    vec3 varying_nrm[3];
    vec3 varying_intensity;
    rayimage shadowbuffer;
    bool has_shadow_map;
};

struct DiffuseShader : public IShader {
  public:
    DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
           vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
           Mat uniform_Mshadow_, bool has_shadow_map);
    
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_intensity; 
    vec3 varying_uv[3];
    vec3 varying_tri[3];
    rayimage shadowbuffer;
    bool has_shadow_map;
    
};

struct DiffuseNormalShader : public IShader {
  DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
               Mat uniform_Mshadow_, bool has_shadow_map);
  
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  rayimage shadowbuffer;
  bool has_shadow_map;
  
};

class DiffuseShaderTangent : public IShader {
  public:
    DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                       vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                       Mat uniform_Mshadow_, bool has_shadow_map);
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_uv[3];
    vec3 varying_tri[4];
    vec3 varying_nrm[3];
    vec3 ndc_tri[3];
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    rayimage shadowbuffer;
    bool has_shadow_map;
    
};

class PhongShader : public IShader {
  public:
    PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                      vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                      Mat uniform_Mshadow_, bool has_shadow_map);
    
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_uv[3];
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    vec3 varying_tri[3];
    vec3 varying_nrm[3];
    rayimage shadowbuffer;
    bool has_shadow_map;
    
};

class PhongNormalShader : public IShader {
public:
  PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
               Mat uniform_Mshadow_, bool has_shadow_map);

  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);

  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_tri[3];
  vec3 varying_uv[3];
  rayimage shadowbuffer;
  bool has_shadow_map;
  
};

class PhongShaderTangent : public IShader {
public:
  PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                     vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                     Mat uniform_Mshadow_, bool has_shadow_map);
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[4];
  vec3 varying_nrm[3];
  vec3 ndc_tri[3];
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  rayimage shadowbuffer;
  bool has_shadow_map;
  
};

struct DepthShader : public IShader {
  DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, ModelInfo& model);
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bar, vec3 &color);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  vec3 varying_nrm[3];  
};

struct PhongShaderShadowMap : public IShader {
  PhongShaderShadowMap(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                  Mat uniform_Mshadow_, bool has_shadow_map);
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bar, vec3 &color);

  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  vec3 varying_nrm[3];
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  rayimage shadowbuffer;
  bool has_shadow_map;
  
};


#endif