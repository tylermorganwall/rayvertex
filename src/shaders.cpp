#include "shaders.h"

GouraudShader::GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                             Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map){
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = MVP;
  uniform_MIT = glm::inverseTranspose(uniform_M);
};

vec3 GouraudShader::vertex(int iface, int nthvert) {
  varying_intensity[nthvert] = std::fmax(0.f, dot(model.normal(iface, nthvert),light_dir));
  varying_nrm[nthvert] = Model * vec4(model.normal(iface, nthvert),0.0f);
  vec4 clip = vp * MVP * vec4(model.vertex(iface, nthvert),1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool GouraudShader::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec3 n = normalize(varying_nrm[0] * bc.x + varying_nrm[1] * bc.y + varying_nrm[2] * bc.z);
    vec3 l = normalize(vec3(Model * vec4(light_dir, 0.0f)));
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = std::fmax(0.01 * (1.0 - std::fabs(dot(n,l))), 0.005);
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  color = shadow * model.color * dot(varying_intensity,bc) * model.diffuse_intensity;
  return(false);
}

DiffuseShader::DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
              Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map){
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
};


vec3 DiffuseShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_intensity[nthvert] = model.has_normals ?
    std::fmax(0.f, dot(vec3(Model * vec4(model.normal(iface, nthvert),0.0f)),light_dir)) : 
    std::fmax(0.f, dot(normalize(
                        glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                   model.vertex(iface,2)-model.vertex(iface,0))),
                       light_dir));
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool DiffuseShader::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  float intensity = dot(varying_intensity,bc);  
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  color = model.has_texture ? model.diffuse(uv)*intensity : model.color * intensity;
  color *= shadow * model.diffuse_intensity;
  return false; 
}

DiffuseNormalShader::DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
             vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
             Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = MVP;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  
};


vec3 DiffuseNormalShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool DiffuseNormalShader::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  vec3 n = normalize(Model * (vec4(model.normal_uv(uv), 0.0f)));
  vec3 l = normalize(Model * (vec4(light_dir, 0.0f)));
  float intensity = std::fmax(0.f, dot(n,l));

  color = model.diffuse(uv) * intensity * shadow * model.diffuse_intensity;
  return false;
}

DiffuseShaderTangent::DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                       vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                                       Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = MVP;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  
}

vec3 DiffuseShaderTangent::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_nrm[nthvert] = Model * vec4(model.normal(iface, nthvert),0.0f);
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 ndc = MVP * vec4(gl_Vertex, 1.0f);
  ndc_tri[nthvert] = ndc/ndc.w;
  vec4 clip = vp*MVP * vec4(gl_Vertex, 1.0f);
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool DiffuseShaderTangent::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  vec3 bn = (varying_nrm[0] * bc.x + 
    varying_nrm[1] * bc.y + 
    varying_nrm[2] * bc.z);
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  
  vec3 edge1 = ndc_tri[1] - ndc_tri[0];
  vec3 edge2 = ndc_tri[2] - ndc_tri[0];
  
  glm::vec2 deltaUV1 = varying_uv[1] - varying_uv[0];
  glm::vec2 deltaUV2 = varying_uv[2] - varying_uv[0];

  float f = 1.0f / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y);
  vec3 T =  deltaUV2.y * edge1 - deltaUV1.y * edge2;
  vec3 B = -deltaUV2.x * edge1 + deltaUV1.x * edge2;

  T = normalize(T);
  B = normalize(B);
  bn = normalize(bn);

  glm::mat3 TBN{T,B,bn};
  
  vec3 n = normalize(TBN * model.normal_uv(uv));
  
  float diff = std::fmax(0.f, dot(n,normalize(vec3(Model * vec4(light_dir,0.0f)))));
  color = model.diffuse(uv)*diff*shadow * model.diffuse_intensity;
  return false;
}

PhongShader::PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                     vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                                     Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  
}

vec3 PhongShader::vertex(int iface, int nthvert) {
  varying_tri[nthvert] = model.vertex(iface,nthvert);
  varying_uv[nthvert] = model.tex(iface,nthvert);
  if(model.has_normals) {
    varying_nrm[nthvert] = model.normal(iface, nthvert);
  }
  vec4 clip = vp * MVP * vec4(varying_tri[nthvert], 1.0f);
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongShader::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  vec3 n = model.has_normals ? normalize(Model * vec4(varying_nrm[0] * bc.x + varying_nrm[1] * bc.y + varying_nrm[2] * bc.z, 0.0f)) :
    normalize(glm::cross(varying_tri[1]-varying_tri[0],varying_tri[2]-varying_tri[0]));
  
  vec3 l = normalize(Model * vec4(light_dir, 0.0f));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = std::pow(std::fmax(r.z, 0.0f),
                        (float)model.specular(uv)*model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] + 
      c[i]*shadow*(diff * model.diffuse_intensity + 
      spec * model.specular_intensity),1);
  }
  return false;
}

PhongNormalShader::PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
            vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
            Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  
}

vec3 PhongNormalShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex, 1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongNormalShader::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;

    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;

  vec3 n = normalize(vec3(Model * vec4(model.normal_uv(uv),0.0f)));
  vec3 l = normalize(vec3(Model * vec4(light_dir, 0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = std::pow(std::fmax(r.z, 0.0f),
                        (float)model.specular(uv)*model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] + 
      c[i]*shadow*(diff * model.diffuse_intensity + 
            spec * model.specular_intensity),1);
  }
  return false;
}

PhongShaderTangent::PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                         Mat uniform_Mshadow_, bool has_shadow_map) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = MVP;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  
}

vec3 PhongShaderTangent::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_nrm[nthvert] = Model * vec4(model.normal(iface, nthvert),0.0f);
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1);
  ndc_tri[nthvert] = clip/clip.w;
  clip = vp * clip;
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongShaderTangent::fragment(vec3 bc, vec3 &color) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] < 0 || sb_p[0] > shadowbuffer.width()-1 || sb_p[1] < 0 || sb_p[1] > shadowbuffer.height()-1) {
      throw std::runtime_error("outside");
    }
    float bias = 0.005;
    shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
  }
  vec3 bn = (varying_nrm[0] * bc.x + 
    varying_nrm[1] * bc.y + 
    varying_nrm[2] * bc.z);
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  
  vec3 edge1 = ndc_tri[1] - ndc_tri[0];
  vec3 edge2 = ndc_tri[2] - ndc_tri[0];
  
  glm::vec2 deltaUV1 = varying_uv[1] - varying_uv[0];
  glm::vec2 deltaUV2 = varying_uv[2] - varying_uv[0];
  
  float f = 1.0f / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y);
  vec3 T =  deltaUV2.y * edge1 - deltaUV1.y * edge2;
  vec3 B = -deltaUV2.x * edge1 + deltaUV1.x * edge2;
  
  T = normalize(T);
  B = normalize(B);
  bn = normalize(bn);
  
  glm::mat3 TBN{T,B,bn};
  
  vec3 n = normalize(TBN * model.normal_uv(uv));
  
  vec3 l = normalize(Model * vec4(light_dir,0.0f));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = std::pow(std::fmax(r.z, 0.0f),
                        (float)model.specular(uv)*model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  vec3 ambient = model.get_ambient();
  
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] +
      c[i]*shadow*(diff * model.diffuse_intensity +
      spec * model.specular_intensity),1);
  }
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

