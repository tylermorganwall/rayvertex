#ifndef SHADERSH
#define SHADERSH

#include "glm.hpp"
#include "model.h"
#include "gtc/matrix_transform.hpp"
#include "gtc/matrix_access.hpp"
#include "gtc/matrix_inverse.hpp"


typedef glm::vec4 vec4;
typedef glm::vec3 vec3;
typedef glm::vec2 vec2;
typedef glm::mat4x4 Mat;

class IShader {
  public:
    virtual vec4 vertex(int iface, int nthvert) = 0;
    virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) = 0;
};



class GouraudShader : public IShader {
  public:
    GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                  Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
    
    virtual vec4 vertex(int iface, int nthvert);
    virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
    
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
    vec3 l;
    ModelInfo model;
    vec3 varying_tri[3];
    vec3 varying_nrm[3];
    vec3 varying_pos[3];
    vec3 varying_world_nrm[3];
    
    vec3 varying_intensity;
    rayimage shadowbuffer;
    bool has_shadow_map;
    float shadow_map_bias;
    
};

struct DiffuseShader : public IShader {
  public:
    DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
           vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
           Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
    
    virtual vec4 vertex(int iface, int nthvert);
    virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    Mat uniform_M;
    Mat uniform_MIT;
    vec4 viewport;
    vec3 light_dir;
    vec3 l;
    
    ModelInfo model;
    vec3 varying_intensity; 
    
    std::vector<vec3> vec_varying_intensity;
    std::vector<std::vector<vec3> > vec_varying_uv;
    std::vector<std::vector<vec3> > vec_varying_tri;
    std::vector<std::vector<vec3> > vec_varying_pos;
    std::vector<std::vector<vec3> > vec_varying_world_nrm;
    
    vec3 varying_uv[3];
    vec3 varying_tri[3];
    vec3 varying_pos[3];
    vec3 varying_world_nrm[3];
    rayimage shadowbuffer;
    bool has_shadow_map;
    float shadow_map_bias;
    
    
};

struct DiffuseNormalShader : public IShader {
  DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
               Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
  
  virtual vec4 vertex(int iface, int nthvert);
  virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
  
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
  vec3 l;
  
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  vec3 varying_pos[3];
  vec3 varying_world_nrm[3];
  
  rayimage shadowbuffer;
  bool has_shadow_map;
  float shadow_map_bias;
  
  
};

class DiffuseShaderTangent : public IShader {
  public:
    DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                       vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                       Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
    virtual vec4 vertex(int iface, int nthvert);
    virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
    vec3 light_dir;
    vec3 l;
    
    ModelInfo model;
    vec3 varying_uv[3];
    vec3 varying_tri[4];
    vec3 varying_nrm[3];
    vec3 ndc_tri[3];
    vec3 varying_pos[3];
    vec3 varying_world_nrm[3];
    
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    rayimage shadowbuffer;
    bool has_shadow_map;
    float shadow_map_bias;
    
    
};

class PhongShader : public IShader {
  public:
    PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                      vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                      Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
    
    virtual vec4 vertex(int iface, int nthvert);
    virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
    vec3 light_dir;
    vec3 l;
    
    ModelInfo model;
    vec3 varying_uv[3];
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    vec3 varying_tri[3];
    vec3 varying_nrm[3];
    vec3 varying_pos[3];
    vec3 varying_world_nrm[3];
    
    rayimage shadowbuffer;
    bool has_shadow_map;
    float shadow_map_bias;
    
    
};

class PhongNormalShader : public IShader {
public:
  PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
               Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);

  virtual vec4 vertex(int iface, int nthvert);
  virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);

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
  vec3 l;
  ModelInfo model;
  vec3 varying_tri[3];
  vec3 varying_uv[3];
  vec3 varying_pos[3];
  vec3 varying_world_nrm[3];
  
  rayimage shadowbuffer;
  bool has_shadow_map;
  float shadow_map_bias;
  
  
};

class PhongShaderTangent : public IShader {
public:
  PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                     vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                     Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
  virtual vec4 vertex(int iface, int nthvert);
  virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  vec4 viewport;
  vec3 light_dir;
  vec3 l;
  
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[4];
  vec3 varying_nrm[3];
  vec3 ndc_tri[3];
  vec3 varying_pos[3];
  vec3 varying_world_nrm[3];
  
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  rayimage shadowbuffer;
  bool has_shadow_map;
  float shadow_map_bias;
  
  
};

struct DepthShader : public IShader {
  DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, ModelInfo& model);
  virtual vec4 vertex(int iface, int nthvert);
  virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  vec4 viewport;
  vec3 light_dir;
  vec3 l;
  
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  vec3 varying_nrm[3];  
  vec3 varying_pos[3];
  vec3 varying_world_nrm[3];
  float shadow_map_bias;
  
  
};

struct PhongShaderShadowMap : public IShader {
  PhongShaderShadowMap(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                  Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias);
  virtual vec4 vertex(int iface, int nthvert);
  virtual bool fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface);

  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  vec4 viewport;
  vec3 light_dir;
  vec3 l;
  
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[3];
  vec3 varying_nrm[3];
  vec3 varying_pos[3];
  vec3 varying_world_nrm[3];
  
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  rayimage shadowbuffer;
  bool has_shadow_map;
  float shadow_map_bias;
  
  
};

#endif