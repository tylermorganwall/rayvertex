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


class GouraudShader : public IShader {
  public:
    GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  vec3 light_dir, ModelInfo& model) :
    Model(Model), Projection(Projection), View(View), viewport(viewport),
    light_dir(light_dir), model(model){};
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    Mat Model;
    Mat Projection;
    Mat View;
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_intensity; // written by vertex shader, read by fragment shader -> specific to Gouraud shadows
};

struct Shader : public IShader {
  public:
    Shader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
           vec3 light_dir, ModelInfo& model) :
    Model(Model), Projection(Projection), View(View), viewport(viewport),
    light_dir(light_dir), model(model){};
    
    virtual vec3 vertex(int iface, int nthvert);
    virtual bool fragment(vec3 bc, vec3 &color);
    
    Mat Model;
    Mat Projection;
    Mat View;
    vec4 viewport;
    vec3 light_dir;
    ModelInfo model;
    vec3 varying_intensity; // written by vertex shader, read by fragment shader -> specific to Gouraud shadows
    vec3 varying_uv[3];
};

struct NormalShader : public IShader {
  NormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model);
  
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);
  
  Mat Model;
  Mat Projection;
  Mat View;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
};

class PhongShader : public IShader {
public:
  PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               vec3 light_dir, ModelInfo& model);

  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);

  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
};

class PhongShaderTangent : public IShader {
public:
  PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                     vec3 light_dir, ModelInfo& model);
  virtual vec3 vertex(int iface, int nthvert);
  virtual bool fragment(vec3 bc, vec3 &color);
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  vec4 viewport;
  vec3 light_dir;
  ModelInfo model;
  vec3 varying_uv[3];
  vec3 varying_tri[4];
  vec3 varying_nrm[3];
  vec3 ndc_tri[3];
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  
};

#endif