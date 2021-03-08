#include "shaders.h"

#include "RcppThread.h"

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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    vec3 temp;
    
    vec_varying_intensity.push_back(temp);
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
  }
};

vec4 GouraudShader::vertex(int iface, int nthvert) {
  vec_varying_intensity[iface][nthvert] = std::fmax(0.f, dot(model.normal(iface, nthvert),light_dir));
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  vec4 clip = vp * MVP * vec4(model.vertex(iface, nthvert),1.0f);
  
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return (clip);
}

bool GouraudShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec3 n = normalize(vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z);
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  color = shadow * model.color * dot(vec_varying_intensity[iface],bc);
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  return(false);
}

DiffuseShader::DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, ModelInfo& model, rayimage& shadowbuffer,
              Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias) :
  Model(Model), Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), model(model), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias){
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = vp * Projection * View * Model;
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    vec3 temp;
    
    vec_varying_intensity.push_back(temp);
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
  }
};


vec4 DiffuseShader::vertex(int iface, int nthvert) {
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
    
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  vec_varying_pos[iface][nthvert] = uniform_MIT * vec4(model.vertex(iface, nthvert),1.0f);
  vec_varying_world_nrm[iface][nthvert] = model.has_normals ?
  uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  vec_varying_intensity[iface][nthvert] = model.has_normals ?
    std::fmax(0.f, dot(normalize(vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f))),l)) : 
    std::fmax(0.f, dot(vec_varying_world_nrm[iface][nthvert], light_dir));
  return (clip);
}

bool DiffuseShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    }
  }
  float intensity = dot(vec_varying_intensity[iface],bc);
  
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  
  color = model.has_texture ? model.diffuse(uv)*intensity : model.color * intensity;
  color *= shadow;
  color += model.emissive(uv);
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    vec3 temp;
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
  }
};


vec4 DiffuseNormalShader::vertex(int iface, int nthvert) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return (clip);
}

bool DiffuseNormalShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv), 0.0f)));
  float intensity = std::fmax(0.f, dot(n,l));
  vec3 emit = model.emissive(uv);
  
  color = emit + model.diffuse(uv) * intensity * shadow;
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    std::vector<vec3> tempndc(3);
    std::vector<vec3> tempnrm2(3);
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
    vec_varying_ndc_tri.push_back(tempndc);
    vec_varying_nrm.push_back(tempnrm2);
  }
}

vec4 DiffuseShaderTangent::vertex(int iface, int nthvert) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);

  vec4 ndc = MVP  * vec4(gl_Vertex, 1.0f);
  vec_varying_ndc_tri[iface][nthvert] = vec3(ndc/ndc.w);
  vec4 clip = vp*MVP * vec4(gl_Vertex, 1.0f);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return clip;
}

bool DiffuseShaderTangent::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 bn = normalize(vec_varying_nrm[iface][0] * bc.x + 
    vec_varying_nrm[iface][1] * bc.y + 
    vec_varying_nrm[iface][2] * bc.z);
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;

  glm::mat3 A{(vec_varying_ndc_tri[iface][1] - vec_varying_ndc_tri[iface][0]),
              (vec_varying_ndc_tri[iface][2] - vec_varying_ndc_tri[iface][0]),bn};
  glm::mat3 AI = inverse(transpose(A));
  vec3 i = AI * vec3(vec_varying_uv[iface][1].x - vec_varying_uv[iface][0].x, 
                     vec_varying_uv[iface][2].x - vec_varying_uv[iface][0].x, 0.0f);
  vec3 j = AI * vec3(vec_varying_uv[iface][1].y - vec_varying_uv[iface][0].y, 
                     vec_varying_uv[iface][2].y - vec_varying_uv[iface][0].y, 0.0f);
  glm::mat3 B = (glm::mat3{ normalize(i), normalize(j), bn });
  vec3 n = normalize(B * model.normal_uv(uv));
  
  vec3 emit = model.emissive(uv);
  
  float diff = std::fmax(0.f, dot(n,l));
  color = emit +  model.diffuse(uv)*diff*shadow;
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    std::vector<vec3> tempndc(3);
    std::vector<vec3> tempnrm2(3);
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
    vec_varying_nrm.push_back(tempnrm2);
  }
}

vec4 PhongShader::vertex(int iface, int nthvert) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);

  if(model.has_normals) {
    vec_varying_world_nrm[iface][nthvert] = uniform_M *vec4(model.normal(iface, nthvert),0.0f);
    vec_varying_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  }
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return clip;
}

bool PhongShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec3 n = model.has_normals ?  normalize(vec_varying_nrm[iface][0] * bc.x + vec_varying_nrm[iface][1] * bc.y + vec_varying_nrm[iface][2] * bc.z) :
    normalize(glm::cross(vec_varying_tri[iface][1]-vec_varying_tri[iface][0],vec_varying_tri[iface][2]-vec_varying_tri[iface][0]));

  vec3 r = normalize(2.0f*dot(n,l)*n - l);
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
      spec * model.specular_intensity),1);
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    std::vector<vec3> tempndc(3);
    std::vector<vec3> tempnrm2(3);
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
  }
  
}

vec4 PhongNormalShader::vertex(int iface, int nthvert) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex, 1.0f);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return clip;
}

bool PhongNormalShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;

  vec3 n = normalize(vec3(uniform_MIT * vec4(model.normal_uv(uv),0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
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
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec3> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    std::vector<vec3> tempndc(3);
    std::vector<vec3> tempnrm2(3);
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
    vec_varying_ndc_tri.push_back(tempndc);
    vec_varying_nrm.push_back(tempnrm2);
  }
}

vec4 PhongShaderTangent::vertex(int iface, int nthvert) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1.0f);
  vec_varying_ndc_tri[iface][nthvert] = clip/clip.w;
  clip = vp * clip;
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  return clip;
}

bool PhongShaderTangent::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      float bias = shadow_map_bias;
      shadow = shadowbuffer.get_color(int(sb_p[0]),int(sb_p[1])).x > sb_p[2]-bias ? 1 : 0;
    } 
  }
  vec3 bn = (vec_varying_nrm[iface][0] * bc.x + 
    vec_varying_nrm[iface][1] * bc.y + 
    vec_varying_nrm[iface][2] * bc.z);
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;

  glm::mat3 A{(vec_varying_ndc_tri[iface][1] - vec_varying_ndc_tri[iface][0]),
              (vec_varying_ndc_tri[iface][2] - vec_varying_ndc_tri[iface][0]),bn};
  glm::mat3 AI = inverse(transpose(A));
  vec3 i = AI * vec3(vec_varying_uv[iface][1].x - vec_varying_uv[iface][0].x, 
                     vec_varying_uv[iface][2].x - vec_varying_uv[iface][0].x, 0.0f);
  vec3 j = AI * vec3(vec_varying_uv[iface][1].y - vec_varying_uv[iface][0].y, 
                     vec_varying_uv[iface][2].y - vec_varying_uv[iface][0].y, 0.0f);
  glm::mat3 B = (glm::mat3{ normalize(i), normalize(j), bn });
  vec3 n = normalize(B * model.normal_uv(uv));
  
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
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
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
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
  for(int i = 0; i < model.inds.nrow(); i++ ) {
    std::vector<vec4> temptri(3);
    vec_varying_tri.push_back(temptri);
  }
}

vec4 DepthShader::vertex(int iface, int nthvert) {
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex,1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  return  clip;
}

bool DepthShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  vec4 p = vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z;
  color = vec3(p.z);
  return false;
}


