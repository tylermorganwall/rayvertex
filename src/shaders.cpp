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
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M));
}

vec3 PhongShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1);
  clip /= clip.w;
  return  vp*clip;// transform it to screen coordinates
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

PhongShaderTangent::PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir, ModelInfo& model) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M));
}

vec3 PhongShaderTangent::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1);
  varying_nrm[nthvert] = vec3(uniform_MIT*vec4(model.normal(iface, nthvert), 0.f));
  varying_tri[nthvert] = clip;
  clip /= clip.w;
  ndc_tri[nthvert] = vec3(clip);
  return vp*clip;
}

bool PhongShaderTangent::fragment(vec3 bc, vec3 &color) {
  vec3 bn = normalize(varying_nrm[0] * bc.x + 
    varying_nrm[1] * bc.y + 
    varying_nrm[2] * bc.z);
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  
  vec3 edge1 = ndc_tri[1] - ndc_tri[0];
  vec3 edge2 = ndc_tri[2] - ndc_tri[0];
  glm::vec2 deltaUV1 = varying_uv[1] - varying_uv[0];
  glm::vec2 deltaUV2 = varying_uv[2] - varying_uv[0];  
  
  float f = 1.0f / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y);
  vec3 T;
  vec3 B;
  T =  deltaUV2.y * edge1 - deltaUV1.y * edge2;
  B = -deltaUV2.x * edge1 + deltaUV1.x * edge2;
  
  T = normalize(T);
  B = normalize(B);
  
  glm::mat3 TBN{T,B,bn};

  vec3 n = glm::normalize(TBN * model.normal_uv(uv));

  float diff = std::max(0.f, dot(n,light_dir));
  color = model.diffuse(uv)*diff;
  return false;
}

DepthShader::DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                       vec3 light_dir, ModelInfo& model) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
}

vec3 DepthShader::vertex(int iface, int nthvert) {
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp*MVP * vec4(gl_Vertex,1);
  varying_tri[nthvert] = clip;
  clip /= clip.w;
  return  clip;// transform it to screen coordinates
}

bool DepthShader::fragment(vec3 bc, vec3 &color) {
  vec3 p = varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z;
  color = vec3(p.z);
  return false;
}

ShadowMapShader::ShadowMapShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                                Mat uniform_Mshadow_) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M));
  uniform_Mshadow = uniform_Mshadow_;
}

vec3 ShadowMapShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp*MVP * vec4(gl_Vertex,1);
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return  clip;
}

bool ShadowMapShader::fragment(vec3 bc, vec3 &color) {
  vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
  sb_p = sb_p/sb_p.w;
  if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
    throw std::runtime_error("outside");
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv), 0.0f)));
  vec3 l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  
  float spec = std::pow(std::fmax(r.z, 0.0f),
                        (float)model.specular(uv)*model.get_exponent());
  float diff = std::max(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  
  float bias = std::fmax(0.1 * (1.0 - std::fabs(dot(n,l))), 0.005);
  float shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0.3;
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] +
      c[i]*shadow*(diff * model.diffuse_intensity +
      spec * model.specular_intensity),1);
  }
  return false;
}
