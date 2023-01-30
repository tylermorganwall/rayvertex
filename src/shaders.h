#ifndef SHADERSH
#define SHADERSH

#include "glm.hpp"
#include "model.h"
#include "gtc/matrix_transform.hpp"
#include "gtc/matrix_access.hpp"
#include "gtc/matrix_inverse.hpp"
#include "light.h"
#include "defines.h"
#include "rayimage.h"

#include "material.h"

static void get_sphere_uv(const vec3& dir, vec2& uv) {
  Float phi = atan2(dir.z, dir.x);
  Float theta = asin(dir.y);
  uv.x = 1 - (phi + M_PI) / (2*M_PI);
  uv.y = (theta + M_PI/2) / M_PI;
}


class IShader {
  public:
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model) = 0;
    virtual bool fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) = 0;
    virtual ~IShader();
    virtual int get_culling() = 0;
    virtual bool is_translucent() = 0;
};



class GouraudShader : public IShader {
  public:
    GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                  bool has_shadow_map, Float shadow_map_bias,
                  material_info mat_info,  std::vector<Light>& point_lights, 
                  std::vector<DirectionalLight> directional_lights, 
                  std::vector<rayimage>& shadowbuffers,
                  std::vector<rayimage>& transparency_buffers,
                  std::vector<vec3>& vec_varying_intensity,
                  std::vector<std::vector<vec3> >& vec_varying_uv,
                  std::vector<std::vector<vec4> >& vec_varying_tri,
                  std::vector<std::vector<vec3> >& vec_varying_pos,
                  std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                  std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                  std::vector<std::vector<vec3> >& vec_varying_nrm,
                  reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
    ~GouraudShader();
    
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
    virtual bool fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface);
    vec3 specular(vec3 uv) {
      return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
    }
    vec4 emissive(vec3 uv) {
      return(has_emissive_texture ? material.emission_intensity * 
             trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
    }
    vec4 diffuse(vec3 uv) {
      return(has_texture ? vec4(material.diffuse_intensity,material.diffuse_intensity,material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
    }
    vec3 ambient(vec3 uv) {
      return(material.has_ambient_texture ? 
             material.ambient * material.ambient_intensity * vec3(trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a))  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
    }
    vec4 reflection(vec3 norm) {
      norm = glm::normalize(norm);
      vec2 uv;
      get_sphere_uv(norm, uv);
      uv.x = 1 - uv.x;
      uv.x += 0.25;
      return(trivalue(uv.x, uv.y, reflection_map));
    }
    int get_culling() {
      return(material.cull_type);
    }
    bool is_translucent() {
      return(material.translucent);
    }
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    Mat uniform_M_inv;
    Mat uniform_MIT_inv;
    vec4 viewport;
    vec3 light_dir;
    vec3 l;
    
    bool has_shadow_map;
    Float shadow_map_bias;
    material_info material;
    
    int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
    float* texture;
    float* ambient_texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;
    
    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    bool has_normals;
    std::vector<Light>& plights;
  
    std::vector<DirectionalLight> directional_lights;
    std::vector<rayimage>& shadowbuffers;
    std::vector<rayimage>& transparency_buffers;
    
    std::vector<vec3>& vec_varying_intensity;
    std::vector<std::vector<vec3> >& vec_varying_uv;
    std::vector<std::vector<vec4> >& vec_varying_tri;
    std::vector<std::vector<vec3> >& vec_varying_pos;
    std::vector<std::vector<vec3> >& vec_varying_world_nrm;
    
    reflection_map_info reflection_map;
    bool has_reflection;
    bool has_refraction;
    
};


class ColorShader : public IShader {
  public:
    ColorShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                material_info mat_info,
                std::vector<vec3>& vec_varying_intensity,
                std::vector<std::vector<vec3> >& vec_varying_uv,
                std::vector<std::vector<vec4> >& vec_varying_tri,
                std::vector<std::vector<vec3> >& vec_varying_pos,
                std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                std::vector<std::vector<vec3> >& vec_varying_nrm,
                reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
    ~ColorShader();
    
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
    virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
    vec3 specular(vec3 uv) {
      return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
    }
    vec4 emissive(vec3 uv) {
      return(has_emissive_texture ? material.emission_intensity * 
             trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
    }
    vec4 diffuse(vec3 uv) {
      return(has_texture ? 
               material.diffuse_intensity * vec4(1.0,1.0,1.0, material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
               vec4(material.diffuse * material.diffuse_intensity, material.dissolve));
    }
    vec4 ambient(vec3 uv) {
      return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
    }
    vec4 reflection(vec3 norm) {
      norm = glm::normalize(norm);
      vec2 uv;
      get_sphere_uv(norm, uv);
      uv.x = 1 - uv.x;
      uv.x += 0.25;
      return(trivalue(uv.x, uv.y, reflection_map));
    }
    int get_culling() {
      return(material.cull_type);
    }
    bool is_translucent() {
      return(material.translucent);
    }
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_M;
    Mat uniform_MIT;
    Mat uniform_M_inv;
    Mat uniform_MIT_inv;
    vec4 viewport;
    
    material_info material;

    int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
    float* texture;
    float* ambient_texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;

    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    
    std::vector<std::vector<vec3> >& vec_varying_uv;
    std::vector<std::vector<vec4> >& vec_varying_tri;
    std::vector<std::vector<vec3> >& vec_varying_pos;
    std::vector<std::vector<vec3> >& vec_varying_world_nrm;
    
    reflection_map_info reflection_map;
    bool has_reflection;
    bool has_refraction;
    
};

class DiffuseShader : public IShader {
  public:
    DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
           bool has_shadow_map, Float shadow_map_bias,
           material_info mat_info,  std::vector<Light>& point_lights, 
           std::vector<DirectionalLight> directional_lights, 
           std::vector<rayimage>& shadowbuffers,
           std::vector<rayimage>& transparency_buffers,
           std::vector<vec3>& vec_varying_intensity,
           std::vector<std::vector<vec3> >& vec_varying_uv,
           std::vector<std::vector<vec4> >& vec_varying_tri,
           std::vector<std::vector<vec3> >& vec_varying_pos,
           std::vector<std::vector<vec3> >& vec_varying_world_nrm,
           std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
           std::vector<std::vector<vec3> >& vec_varying_nrm,
           reflection_map_info reflection_map, bool has_reflection, bool has_refraction,
           bool two_sided);
    ~DiffuseShader();
    
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
    virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
    vec3 specular(vec3 uv) {
      return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
    }
    vec4 emissive(vec3 uv) {
      return(has_emissive_texture ? material.emission_intensity * 
             trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
    }
    vec4 diffuse(vec3 uv) {
      return(has_texture ? 
               vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
               vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
    }
    vec4 ambient(vec3 uv) {
      return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
    }
    vec4 reflection(vec3 norm) {
      norm = glm::normalize(norm);
      vec2 uv;
      get_sphere_uv(norm, uv);
      uv.x = 1 - uv.x;
      uv.x += 0.25;
      return(trivalue(uv.x, uv.y, reflection_map));
    }
    int get_culling() {
      return(material.cull_type);
    }
    bool is_translucent() {
      return(material.translucent);
    }
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    Mat uniform_M;
    Mat uniform_MIT;
    Mat uniform_M_inv;
    Mat uniform_MIT_inv;
    
    vec4 viewport;
    
    bool has_shadow_map;
    Float shadow_map_bias;
    material_info material;
    
    int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
    float* texture;
    float* ambient_texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;

    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    bool has_normals;
    
    std::vector<Light>& plights;
    std::vector<DirectionalLight> directional_lights;
    std::vector<rayimage>& shadowbuffers;
    std::vector<rayimage>& transparency_buffers;
    
    std::vector<vec3>& vec_varying_intensity;
    std::vector<std::vector<vec3> >& vec_varying_uv;
    std::vector<std::vector<vec4> >& vec_varying_tri;
    std::vector<std::vector<vec3> >& vec_varying_pos;
    std::vector<std::vector<vec3> >& vec_varying_world_nrm;
    
    
    reflection_map_info reflection_map;
    bool has_reflection;   
    bool has_refraction;  
    bool two_sided;
};

class DiffuseNormalShader : public IShader {
public:
  DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               
               bool has_shadow_map, Float shadow_map_bias,
               material_info mat_info,  std::vector<Light>& point_lights, 
               std::vector<DirectionalLight> directional_lights, 
               std::vector<rayimage>& shadowbuffers,
               std::vector<rayimage>& transparency_buffers,
               std::vector<vec3>& vec_varying_intensity,
               std::vector<std::vector<vec3> >& vec_varying_uv,
               std::vector<std::vector<vec4> >& vec_varying_tri,
               std::vector<std::vector<vec3> >& vec_varying_pos,
               std::vector<std::vector<vec3> >& vec_varying_world_nrm,
               std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
               std::vector<std::vector<vec3> >& vec_varying_nrm,
               reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
  ~DiffuseNormalShader();
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec3 specular(vec3 uv) {
    return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
  }
  vec4 emissive(vec3 uv) {
    return(has_emissive_texture ? material.emission_intensity * 
           trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
  }
  vec3 normal_uv(vec3 uv) {
    return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
  }
  vec4 diffuse(vec3 uv) {
    return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  vec4 ambient(vec3 uv) {
    return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
  }
  vec4 reflection(vec3 norm) {
    norm = glm::normalize(norm);
    vec2 uv;
    get_sphere_uv(norm, uv);
    uv.x = 1 - uv.x;
    uv.x += 0.25;
    return(trivalue(uv.x, uv.y, reflection_map));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  Mat uniform_M_inv;
  Mat uniform_MIT_inv;
  vec4 viewport;
  
  bool has_shadow_map;
  Float shadow_map_bias;
  material_info material;
  
  
  int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
  float* texture;
  float* ambient_texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;

  bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
  bool has_normals;
  
  std::vector<Light>& plights;
  std::vector<DirectionalLight> directional_lights;
  std::vector<rayimage>& shadowbuffers;
  std::vector<rayimage>& transparency_buffers;
  
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  std::vector<std::vector<vec3> >& vec_varying_pos;
  std::vector<std::vector<vec3> >& vec_varying_world_nrm;
  
  
  reflection_map_info reflection_map;
  bool has_reflection;   
  bool has_refraction;  
  
};

class DiffuseShaderTangent : public IShader {
  public:
    DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                       
                       bool has_shadow_map, Float shadow_map_bias,
                       material_info mat_info,  std::vector<Light>& point_lights, 
                       std::vector<DirectionalLight> directional_lights, 
                       std::vector<rayimage>& shadowbuffers,
                       std::vector<rayimage>& transparency_buffers,
                       std::vector<vec3>& vec_varying_intensity,
                       std::vector<std::vector<vec3> >& vec_varying_uv,
                       std::vector<std::vector<vec4> >& vec_varying_tri,
                       std::vector<std::vector<vec3> >& vec_varying_pos,
                       std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                       std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                       std::vector<std::vector<vec3> >& vec_varying_nrm,
                       reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
    ~DiffuseShaderTangent();
    vec3 specular(vec3 uv) {
      return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
    }
    vec4 emissive(vec3 uv) {
      return(has_emissive_texture ? material.emission_intensity * 
             trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
    }
    vec4 diffuse(vec3 uv) {
      return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
               vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
    }
    vec4 ambient(vec3 uv) {
      return(material.has_ambient_texture ? 
               vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
               vec4(material.ambient * material.ambient_intensity,0.0f));
    }
    vec4 reflection(vec3 norm) {
      norm = glm::normalize(norm);
      vec2 uv;
      get_sphere_uv(norm, uv);
      uv.x = 1 - uv.x;
      uv.x += 0.25;
      return(trivalue(uv.x, uv.y, reflection_map));
    }
    int get_culling() {
      return(material.cull_type);
    }
    bool is_translucent() {
      return(material.translucent);
    }
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
    virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
    
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
  
    
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    Mat uniform_M_inv;
    Mat uniform_MIT_inv;
    
    bool has_shadow_map;
    Float shadow_map_bias;
    material_info material;
    
    int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
    float* texture;
    float* ambient_texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;

    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    bool has_normals;
    
    std::vector<Light>& plights;
    std::vector<DirectionalLight> directional_lights;
    std::vector<rayimage>& shadowbuffers;
    std::vector<rayimage>& transparency_buffers;
    
    std::vector<vec3>& vec_varying_intensity;
    std::vector<std::vector<vec3> >& vec_varying_uv;
    std::vector<std::vector<vec4> >& vec_varying_tri;
    std::vector<std::vector<vec3> >& vec_varying_pos;
    std::vector<std::vector<vec3> >& vec_varying_ndc_tri;
    std::vector<std::vector<vec3> >& vec_varying_world_nrm;
    std::vector<std::vector<vec3> >& vec_varying_nrm;
    
    
    reflection_map_info reflection_map;
    bool has_reflection;    
    bool has_refraction;   
    
};

class PhongShader : public IShader {
  public:
    PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                      bool has_shadow_map, Float shadow_map_bias,
                      material_info mat_info,  std::vector<Light>& point_lights, 
                      std::vector<DirectionalLight> directional_lights, 
                      std::vector<rayimage>& shadowbuffers,
                      std::vector<rayimage>& transparency_buffers,
                      std::vector<vec3>& vec_varying_intensity,
                      std::vector<std::vector<vec3> >& vec_varying_uv,
                      std::vector<std::vector<vec4> >& vec_varying_tri,
                      std::vector<std::vector<vec3> >& vec_varying_pos,
                      std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                      std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                      std::vector<std::vector<vec3> >& vec_varying_nrm,
                      reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
    ~PhongShader();
    
    virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
    virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
    vec3 specular(vec3 uv) {
      return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  
               material.specular_intensity * material.specular);
    }
    vec4 emissive(vec3 uv) {
      return(has_emissive_texture ? material.emission_intensity * 
             trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
    }
    vec3 normal_uv(vec3 uv) {
      return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
    }
    vec4 diffuse(vec3 uv) {
      return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
               vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
    }
    vec4 ambient(vec3 uv) {
      return(material.has_ambient_texture ? 
               vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
               vec4(material.ambient * material.ambient_intensity,0.0f));
    }
    vec4 reflection(vec3 norm) {
      norm = glm::normalize(norm);
      vec2 uv;
      get_sphere_uv(norm, uv);
      uv.x = 1 - uv.x;
      uv.x += 0.25;
      return(trivalue(uv.x, uv.y, reflection_map));
    }
    int get_culling() {
      return(material.cull_type);
    }
    bool is_translucent() {
      return(material.translucent);
    }
    Mat Model;
    Mat Projection;
    Mat View;
    Mat MVP;
    Mat vp;
    Mat uniform_Mshadow;
    vec4 viewport;
    
    Mat uniform_M;   //  Projection*ModelView
    Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
    Mat uniform_M_inv;
    Mat uniform_MIT_inv;
    
    bool has_shadow_map;
    Float shadow_map_bias;
    material_info material;
    
    int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
    float* texture;
    float* ambient_texture;
    float* normal_texture;
    float* specular_texture;
    float* emissive_texture;

    bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
    bool has_normals;
    
    std::vector<Light>& plights;
    std::vector<DirectionalLight> directional_lights;
    std::vector<rayimage>& shadowbuffers;
    std::vector<rayimage>& transparency_buffers;
    
    std::vector<vec3>& vec_varying_intensity;
    std::vector<std::vector<vec3> >& vec_varying_uv;
    std::vector<std::vector<vec4> >& vec_varying_tri;
    std::vector<std::vector<vec3> >& vec_varying_nrm;
    std::vector<std::vector<vec3> >& vec_varying_pos;
    std::vector<std::vector<vec3> >& vec_varying_world_nrm;
    
    
    reflection_map_info reflection_map;
    bool has_reflection;   
    bool has_refraction;  
      
    
};

class PhongNormalShader : public IShader {
public:
  PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
               
               bool has_shadow_map, Float shadow_map_bias,
               material_info mat_info,  std::vector<Light>& point_lights, 
               std::vector<DirectionalLight> directional_lights, 
               std::vector<rayimage>& shadowbuffers,
               std::vector<rayimage>& transparency_buffers,
               std::vector<vec3>& vec_varying_intensity,
               std::vector<std::vector<vec3> >& vec_varying_uv,
               std::vector<std::vector<vec4> >& vec_varying_tri,
               std::vector<std::vector<vec3> >& vec_varying_pos,
               std::vector<std::vector<vec3> >& vec_varying_world_nrm,
               std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
               std::vector<std::vector<vec3> >& vec_varying_nrm,
               reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
  ~PhongNormalShader();
  
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec3 specular(vec3 uv) {
    return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
  }
  vec4 emissive(vec3 uv) {
    return(has_emissive_texture ? material.emission_intensity * 
           trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
  }
  vec3 normal_uv(vec3 uv) {
    return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
  }
  vec4 diffuse(vec3 uv) {
    return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  vec4 ambient(vec3 uv) {
    return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
  }
  vec4 reflection(vec3 norm) {
    norm = glm::normalize(norm);
    vec2 uv;
    get_sphere_uv(norm, uv);
    uv.x = 1 - uv.x;
    uv.x += 0.25;
    return(trivalue(uv.x, uv.y, reflection_map));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  Mat uniform_M_inv;
  Mat uniform_MIT_inv;
  vec4 viewport;
  
  bool has_shadow_map;
  Float shadow_map_bias;
  material_info material;
  
  int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
  float* texture;
  float* ambient_texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;

  bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
  bool has_normals;
  
  std::vector<Light>& plights;
  std::vector<DirectionalLight> directional_lights;
  std::vector<rayimage>& shadowbuffers;
  std::vector<rayimage>& transparency_buffers;
  
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  std::vector<std::vector<vec3> >& vec_varying_pos;
  std::vector<std::vector<vec3> >& vec_varying_world_nrm;
  
  
  reflection_map_info reflection_map;
  bool has_reflection;   
  bool has_refraction;  
  
  
};

class PhongShaderTangent : public IShader {
public:
  PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                     
                     bool has_shadow_map, Float shadow_map_bias,
                     material_info mat_info,  std::vector<Light>& point_lights, 
                     std::vector<DirectionalLight> directional_lights, 
                     std::vector<rayimage>& shadowbuffers,
                     std::vector<rayimage>& transparency_buffers,
                     std::vector<vec3>& vec_varying_intensity,
                     std::vector<std::vector<vec3> >& vec_varying_uv,
                     std::vector<std::vector<vec4> >& vec_varying_tri,
                     std::vector<std::vector<vec3> >& vec_varying_pos,
                     std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                     std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                     std::vector<std::vector<vec3> >& vec_varying_nrm,
                     reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
  ~PhongShaderTangent();
  
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec3 specular(vec3 uv) {
    return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  
             material.specular_intensity * material.specular);
  }
  vec4 emissive(vec3 uv) {
    return(has_emissive_texture ? material.emission_intensity * 
           trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
  }
  vec3 normal_uv(vec3 uv) {
    return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
  }
  vec4 diffuse(vec3 uv) {
    return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  vec4 ambient(vec3 uv) {
    return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
  }
  vec4 reflection(vec3 norm) {
    norm = glm::normalize(norm);
    vec2 uv;
    get_sphere_uv(norm, uv);
    uv.x = 1 - uv.x;
    uv.x += 0.25;
    return(trivalue(uv.x, uv.y, reflection_map));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  vec4 viewport;
  
  Mat uniform_M;   //  Projection*ModelView
  Mat uniform_MIT; // (Projection*ModelView).invert_transpose()
  Mat uniform_M_inv;
  Mat uniform_MIT_inv;
  
  bool has_shadow_map;
  Float shadow_map_bias;
  material_info material;
  
  int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
  float* texture;
  float* ambient_texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;

  bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
  bool has_normals;
  
  std::vector<Light>& plights;
  std::vector<DirectionalLight> directional_lights;
  std::vector<rayimage>& shadowbuffers;
  std::vector<rayimage>& transparency_buffers;
  
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  std::vector<std::vector<vec3> >& vec_varying_pos;
  std::vector<std::vector<vec3> >& vec_varying_ndc_tri;
  std::vector<std::vector<vec3> >& vec_varying_world_nrm;
  std::vector<std::vector<vec3> >& vec_varying_nrm;
  
  
  reflection_map_info reflection_map;
  bool has_reflection;   
  bool has_refraction;  
  
};

//
//Simple shader just for shadow maps
//

class DepthShader : public IShader {
public:
  DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              material_info mat_info, int mat_ind,
              std::vector<std::vector<vec3> >& vec_varying_uv,
              std::vector<std::vector<vec4> >& vec_varying_tri
              );
  ~DepthShader();
  
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec4 diffuse(vec3 uv) {
    return(has_texture ? vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
                         vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  vec4 viewport;
  
  int nx_t, ny_t, nn_t;
  float* texture;
  
  material_info material;
  
  Float shadow_map_bias;
  bool has_texture;
  
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  
  
};


class ToonShader : public IShader {
public:
  ToonShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                bool has_shadow_map, Float shadow_map_bias,
                material_info mat_info,  std::vector<Light>& point_lights, 
                std::vector<DirectionalLight> directional_lights, 
                std::vector<rayimage>& shadowbuffers,
                std::vector<rayimage>& transparency_buffers,
                std::vector<vec3>& vec_varying_intensity,
                std::vector<std::vector<vec3> >& vec_varying_uv,
                std::vector<std::vector<vec4> >& vec_varying_tri,
                std::vector<std::vector<vec3> >& vec_varying_pos,
                std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                std::vector<std::vector<vec3> >& vec_varying_nrm,
                reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
  ~ToonShader();
  
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec3 specular(vec3 uv) {
    return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
  }
  vec4 emissive(vec3 uv) {
    return(has_emissive_texture ? material.emission_intensity * 
           trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
  }
  vec3 normal_uv(vec3 uv) {
    return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
  }
  vec4 diffuse(vec3 uv) {
    return(has_texture ? 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  vec4 ambient(vec3 uv) {
    return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
  }
  vec4 reflection(vec3 norm) {
    norm = glm::normalize(norm);
    vec2 uv;
    get_sphere_uv(norm, uv);
    uv.x = 1 - uv.x;
    uv.x += 0.25;
    return(trivalue(uv.x, uv.y, reflection_map));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;
  Mat uniform_MIT;
  Mat uniform_M_inv;
  Mat uniform_MIT_inv;
  vec4 viewport;
  
  bool has_shadow_map;
  Float shadow_map_bias;
  material_info material;
  
  int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
  float* texture;
  float* ambient_texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;

  bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
  bool has_normals;
  
  std::vector<Light>& plights;
  std::vector<DirectionalLight> directional_lights;
  std::vector<rayimage>& shadowbuffers;
  std::vector<rayimage>& transparency_buffers;
  
  std::vector<vec3>& vec_varying_intensity;
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  std::vector<std::vector<vec3> >& vec_varying_pos;
  std::vector<std::vector<vec3> >& vec_varying_world_nrm;
  
  
  reflection_map_info reflection_map;
  bool has_reflection;   
  bool has_refraction;  
  
};

class ToonShaderPhong : public IShader {
public:
  ToonShaderPhong(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
             bool has_shadow_map, Float shadow_map_bias,
             material_info mat_info,  std::vector<Light>& point_lights, 
             std::vector<DirectionalLight> directional_lights, 
             std::vector<rayimage>& shadowbuffers,
             std::vector<rayimage>& transparency_buffers,
             std::vector<vec3>& vec_varying_intensity,
             std::vector<std::vector<vec3> >& vec_varying_uv,
             std::vector<std::vector<vec4> >& vec_varying_tri,
             std::vector<std::vector<vec3> >& vec_varying_pos,
             std::vector<std::vector<vec3> >& vec_varying_world_nrm,
             std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
             std::vector<std::vector<vec3> >& vec_varying_nrm,
             reflection_map_info reflection_map, bool has_reflection, bool has_refraction);
  ~ToonShaderPhong();
  
  virtual vec4 vertex(int iface, int nthvert, ModelInfo& model);
  virtual bool fragment(const vec3& bc,vec4 &color, vec3& pos, vec3& normal, int iface);
  vec3 specular(vec3 uv) {
    return(has_specular_texture ? material.specular_intensity * trivalue(uv.x,uv.y,specular_texture, nx_st, ny_st, nn_st) :  material.specular_intensity * material.specular);
  }
  vec4 emissive(vec3 uv) {
    return(has_emissive_texture ? material.emission_intensity * 
           trivalue(uv.x,uv.y,emissive_texture, nx_et, ny_et, nn_et) : vec4(0.0f));
  }
  vec3 normal_uv(vec3 uv) {
    return(trivalue(uv.x, uv.y, normal_texture, nx_nt, ny_nt, nn_nt)*(Float)2 - (Float)1);
  }
  vec4 diffuse(vec3 uv) {
    return(has_texture ? 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve) * trivalue(uv.x,uv.y,texture, nx_t, ny_t, nn_t)  : 
             vec4(material.diffuse * material.diffuse_intensity,material.dissolve));
  }
  vec4 ambient(vec3 uv) {
    return(material.has_ambient_texture ? 
             vec4(material.ambient * material.ambient_intensity,0.0f) * trivalue(uv.x,uv.y,ambient_texture, nx_a, ny_a, nn_a)  : 
             vec4(material.ambient * material.ambient_intensity,0.0f));
  }
  vec4 reflection(vec3 norm) {
    norm = glm::normalize(norm);
    vec2 uv;
    get_sphere_uv(norm, uv);
    uv.x = 1 - uv.x;
    uv.x += 0.25;
    return(trivalue(uv.x, uv.y, reflection_map));
  }
  int get_culling() {
    return(material.cull_type);
  }
  bool is_translucent() {
    return(material.translucent);
  }
  
  Mat Model;
  Mat Projection;
  Mat View;
  Mat MVP;
  Mat vp;
  Mat uniform_Mshadow;
  Mat uniform_M;
  Mat uniform_MIT;
  Mat uniform_M_inv;
  Mat uniform_MIT_inv;
  vec4 viewport;
  
  bool has_shadow_map;
  Float shadow_map_bias;
  material_info material;
  
  int nx_t, ny_t, nn_t, nx_a, ny_a, nn_a,  nx_nt, ny_nt, nn_nt, nx_st, ny_st, nn_st, nx_et, ny_et, nn_et;
  float* texture;
  float* ambient_texture;
  float* normal_texture;
  float* specular_texture;
  float* emissive_texture;

  bool has_texture, has_normal_texture, has_specular_texture, has_emissive_texture;
  bool has_normals;
  
  std::vector<Light>& plights;
  std::vector<DirectionalLight> directional_lights;
  std::vector<rayimage>& shadowbuffers;
  std::vector<rayimage>& transparency_buffers;
  
  std::vector<vec3>& vec_varying_intensity;
  std::vector<std::vector<vec3> >& vec_varying_uv;
  std::vector<std::vector<vec4> >& vec_varying_tri;
  std::vector<std::vector<vec3> >& vec_varying_pos;
  std::vector<std::vector<vec3> >& vec_varying_world_nrm;
  
  
  reflection_map_info reflection_map;
  bool has_reflection;  
  bool has_refraction;  
  
};



#endif
