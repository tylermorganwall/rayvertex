#include "shaders.h"

GouraudShader::GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                             Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
};

vec3 GouraudShader::vertex(int iface, int nthvert) {
  varying_intensity[nthvert] = std::fmax(0.f, dot(model.normal(iface, nthvert),light_dir));
  varying_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  varying_pos[nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  varying_world_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));

  vec4 clip = vp * MVP * vec4(model.vertex(iface, nthvert),1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool GouraudShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec3 n = normalize(varying_nrm[0] * bc.x + varying_nrm[1] * bc.y + varying_nrm[2] * bc.z);
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  color = shadow * model.color * dot(varying_intensity,bc);
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  varying_nrm[0] * bc.x + varying_nrm[1] * bc.y + varying_nrm[2] * bc.z;;
  return(false);
}

DiffuseShader::DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
              Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias){
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
};


vec3 DiffuseShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  varying_pos[nthvert] = uniform_MIT * vec4(model.vertex(iface, nthvert),1.0f);
  varying_world_nrm[nthvert] = View * Model * vec4(model.normal(iface, nthvert),0.0f);
  varying_intensity[nthvert] = model.has_normals ?
    std::fmax(0.f, dot(normalize(vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f))),l)) : 
    std::fmax(0.f, dot(normalize(
        glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                   model.vertex(iface,2)-model.vertex(iface,0))),
                   light_dir));
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool DiffuseShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  float intensity = dot(varying_intensity,bc);  
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  color = model.has_texture ? model.diffuse(uv)*intensity : model.color * intensity;
  color *= shadow;
  color += model.emissive(uv);
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  varying_world_nrm[0] * bc.x + varying_world_nrm[1] * bc.y + varying_world_nrm[2] * bc.z;;
  
  return false; 
}

DiffuseNormalShader::DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
             vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
             Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
};


vec3 DiffuseNormalShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_pos[nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  varying_world_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return (clip);
}

bool DiffuseNormalShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv), 0.0f)));
  float intensity = std::fmax(0.f, dot(n,l));
  vec3 emit = model.emissive(uv);
  
  color = emit + model.diffuse(uv) * intensity * shadow;
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal = n;
  return false;
}

DiffuseShaderTangent::DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                       vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                                       Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
}

vec3 DiffuseShaderTangent::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  varying_pos[nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  varying_world_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);

  vec4 ndc = MVP  * vec4(gl_Vertex, 1.0f);
  ndc_tri[nthvert] = vec3(ndc/ndc.w);
  vec4 clip = vp*MVP * vec4(gl_Vertex, 1.0f);
  clip /= clip.w;
  varying_tri[nthvert] = vec3(clip);
  return clip;
}

bool DiffuseShaderTangent::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 bn = normalize(varying_nrm[0] * bc.x + 
    varying_nrm[1] * bc.y + 
    varying_nrm[2] * bc.z);
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;

  glm::mat3 A{(ndc_tri[1] - ndc_tri[0]),(ndc_tri[2] - ndc_tri[0]),bn};
  glm::mat3 AI = inverse(transpose(A));
  vec3 i = AI * vec3(varying_uv[1].x - varying_uv[0].x, varying_uv[2].x - varying_uv[0].x, 0.0f);
  vec3 j = AI * vec3(varying_uv[1].y - varying_uv[0].y, varying_uv[2].y - varying_uv[0].y, 0.0f);
  glm::mat3 B = (glm::mat3{ normalize(i), normalize(j), bn });
  vec3 n = normalize(B * model.normal_uv(uv));
  
  vec3 emit = model.emissive(uv);
  float diff = std::fmax(0.f, dot(n,l));
  color = emit +  model.diffuse(uv)*diff*shadow;
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  n;
  
  return false;
}

PhongShader::PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                     vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                                     Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverse(glm::transpose(uniform_M));
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
}

vec3 PhongShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_pos[nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);

  if(model.has_normals) {
    varying_world_nrm[nthvert] = uniform_M *vec4(model.normal(iface, nthvert),0.0f);
    varying_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  }
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;
  vec3 n = model.has_normals ?  normalize(varying_nrm[0] * bc.x + varying_nrm[1] * bc.y + varying_nrm[2] * bc.z) :
    normalize(glm::cross(varying_tri[1]-varying_tri[0],varying_tri[2]-varying_tri[0]));

  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = (float)model.specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  
  for (int i=0; i<3; i++) {
    color[i] = std::fmin(ambient[i] + 
      c[i]*shadow*(diff + 
      spec * model.specular_intensity),1);
  }
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  varying_world_nrm[0] * bc.x + varying_world_nrm[1] * bc.y + varying_world_nrm[2] * bc.z;;
  
  return false;
}

PhongNormalShader::PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
            vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
            Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M)); 
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
}

vec3 PhongNormalShader::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_pos[nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  varying_world_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex, 1.0f);
  clip = clip/clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongNormalShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;

  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv),0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = (float)model.specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                        model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  color = c;
  vec3 ambient = model.get_ambient();
  vec3 emit = model.emissive(uv);
  
  for (int i=0; i<3; i++) {
    color[i] = emit[i] + std::fmin(ambient[i] + 
      c[i]*shadow*(diff + 
            spec * model.specular_intensity*model.specular_color[i]),1);
  }
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  varying_world_nrm[0] * bc.x + varying_world_nrm[1] * bc.y + varying_world_nrm[2] * bc.z;;
  
  return false;
}

PhongShaderTangent::PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
                         Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
}

vec3 PhongShaderTangent::vertex(int iface, int nthvert) {
  varying_uv[nthvert] = model.tex(iface,nthvert);
  varying_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  varying_pos[nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  varying_world_nrm[nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1.0f);
  ndc_tri[nthvert] = clip/clip.w;
  clip = vp * clip;
  clip /= clip.w;
  varying_tri[nthvert] = clip;
  return clip;
}

bool PhongShaderTangent::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z, 1.0f); // corresponding point in the shadow buffer
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 bn = (varying_nrm[0] * bc.x + 
    varying_nrm[1] * bc.y + 
    varying_nrm[2] * bc.z);
  vec3 uv = varying_uv[0] * bc.x + varying_uv[1] * bc.y + varying_uv[2] * bc.z;

  glm::mat3 A{(ndc_tri[1] - ndc_tri[0]),(ndc_tri[2] - ndc_tri[0]),bn};
  glm::mat3 AI = inverse(transpose(A));
  vec3 i = AI * vec3(varying_uv[1].x - varying_uv[0].x, varying_uv[2].x - varying_uv[0].x, 0.0f);
  vec3 j = AI * vec3(varying_uv[1].y - varying_uv[0].y, varying_uv[2].y - varying_uv[0].y, 0.0f);
  glm::mat3 B = (glm::mat3{ normalize(i), normalize(j), bn });
  vec3 n = normalize(B * model.normal_uv(uv));
  
  vec3 r = normalize(2.0f*dot(n,l)*n - l);   // reflected light
  float spec = (float)model.specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                model.get_exponent());
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = model.diffuse(uv);
  vec3 ambient = model.get_ambient();
  
  vec3 emit = model.emissive(uv);
  for (int i=0; i<3; i++) {
    color[i] = emit[i] + std::fmin(ambient[i] +
      c[i]*shadow*(diff +
      spec * model.specular_intensity * model.specular_color[i]),1);
  }
  pos =  varying_pos[0] * bc.x + varying_pos[1] * bc.y + varying_pos[2] * bc.z;;
  normal =  varying_world_nrm[0] * bc.x + varying_world_nrm[1] * bc.y + varying_world_nrm[2] * bc.z;;
  
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

bool DepthShader::fragment(vec3 bc, vec3 &color, vec3& pos, vec3& normal) {
  vec3 p = varying_tri[0] * bc.x + varying_tri[1] * bc.y + varying_tri[2] * bc.z;
  color = vec3(p.z);
  return false;
}


