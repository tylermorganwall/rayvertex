#include "shaders.h"


vec3 GouraudShader::vertex(int iface, int nthvert) {
  varying_intensity[nthvert] = std::fmax(0.f, dot(model.normal(iface, nthvert),light_dir));
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  return(glm::project(gl_Vertex, Model, Projection * View, viewport));
}
bool GouraudShader::fragment(vec3 bc, vec3 &color) {
  color = vec3(1) * dot(varying_intensity,bc);
  return(false);
}


vec3 Shader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_intensity[nthvert] = std::fmax(0.f, dot(model.normal(iface, nthvert),light_dir)); // get diffuse lighting intensity
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  return glm::project(gl_Vertex, Model, Projection * View, viewport); // transform it to screen coordinates
}

bool Shader::fragment(vec3 bc, vec3 &color) {
  // interpolate intensity for the current pixel
  float intensity = dot(varying_intensity,bc);  
  // interpolate uv for the current pixel
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  color = model.diffuse(uv)*intensity; 
  return false; 
}

NormalShader::NormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
             vec3 light_dir, ModelInfo& model) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model) {
  uniform_M = Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M));
};


vec3 NormalShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  return glm::project(gl_Vertex, Model, Projection * View, viewport); // transform it to screen coordinates
}

bool NormalShader::fragment(vec3 bc, vec3 &color) {
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  

  vec3 n = normalize(uniform_MIT * vec4(model.normal_uv(uv),0));
  vec3 l = normalize(uniform_M * vec4(light_dir,0));
  
  float intensity = std::max(0.f, dot(n,l));
  color = model.diffuse(uv)*intensity;

  return false;
}

PhongShader::PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
            vec3 light_dir, ModelInfo& model) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model) {
  uniform_M = Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M));
}

vec3 PhongShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  return glm::project(gl_Vertex, Model, Projection * View, viewport); // transform it to screen coordinates
}

bool PhongShader::fragment(vec3 bc, vec3 &color) {
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;

  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv),1)));
  vec3 l = normalize(vec3(uniform_M * vec4(light_dir,1)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = std::pow(std::fmax(r.z, 0.0f), 
                        (float)model.specular(uv)*model.get_exponent());
  float diff = std::max(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] + 
      c[i]*(diff * model.diffuse_intensity + 
            spec * model.specular_intensity),1);
  }
  return false;
}
