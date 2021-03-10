#include "shaders.h"

#include "RcppThread.h"
#include "stb_image.h"

IShader::~IShader() {}

GouraudShader::GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             vec3 light_dir, rayimage& shadowbuffer,
                             Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
                             material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  for(int i = 0; i < material.max_indices; i++ ) {
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

GouraudShader::~GouraudShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

vec4 GouraudShader::vertex(int iface, int nthvert, ModelInfo& model) {
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
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
    }
  }
  
  color = shadow * material.diffuse * dot(vec_varying_intensity[iface],bc);
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  return(false);
}

DiffuseShader::~DiffuseShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

DiffuseShader::DiffuseShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
              vec3 light_dir, rayimage& shadowbuffer,
              Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
              material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info){
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
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


vec4 DiffuseShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] = clip/clip.w;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  vec_varying_world_nrm[iface][nthvert] = model.has_normals ?
    uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  vec_varying_intensity[iface][nthvert] = std::fmax(0.f, dot(vec_varying_world_nrm[iface][nthvert], l));
  return (clip);
}

bool DiffuseShader::fragment(const vec3& bc, vec3 &color, vec3& pos, vec3& normal, int iface) {
  float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * vec4(vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z, 1.0f);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
    }
  }
  float intensity = dot(vec_varying_intensity[iface],bc);
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  
  color = has_texture ? diffuse(uv)*intensity : material.diffuse * intensity * material.diffuse_intensity;
  color *= shadow;
  color += emissive(uv);
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  
  return false; 
}

DiffuseNormalShader::~DiffuseNormalShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

DiffuseNormalShader::DiffuseNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
             vec3 light_dir, rayimage& shadowbuffer,
             Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
             material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  for(int i = 0; i < material.max_indices; i++ ) {
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


vec4 DiffuseNormalShader::vertex(int iface, int nthvert, ModelInfo& model) {
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
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
    }
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec3 n = normalize(vec3(uniform_MIT * vec4(normal_uv(uv), 0.0f)));
  float intensity = std::fmax(0.f, dot(n,l));
  vec3 emit = emissive(uv);
  
  color = emit + diffuse(uv) * intensity * shadow;
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal = n;
  return false;
}

DiffuseShaderTangent::~DiffuseShaderTangent() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

DiffuseShaderTangent::DiffuseShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                       vec3 light_dir, rayimage& shadowbuffer,
                                       Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
                                       material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
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

vec4 DiffuseShaderTangent::vertex(int iface, int nthvert, ModelInfo& model) {
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
  vec3 n = normalize(B * normal_uv(uv));
  
  vec3 emit = emissive(uv);
  
  float diff = std::fmax(0.f, dot(n,l));
  color = emit +  diffuse(uv)*diff*shadow;
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  normal =  n;
  
  return false;
}

PhongShader::~PhongShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

PhongShader::PhongShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                                     vec3 light_dir, rayimage& shadowbuffer,
                                     Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
                                     material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverse(glm::transpose(uniform_M));
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  for(int i = 0; i < material.max_indices; i++ ) {
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

vec4 PhongShader::vertex(int iface, int nthvert, ModelInfo& model) {
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
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
    }
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec3 n = material.has_normals ?  normalize(vec_varying_nrm[iface][0] * bc.x + vec_varying_nrm[iface][1] * bc.y + vec_varying_nrm[iface][2] * bc.z) :
    normalize(glm::cross(vec_varying_tri[iface][1]-vec_varying_tri[iface][0],vec_varying_tri[iface][2]-vec_varying_tri[iface][0]));

  vec3 r = normalize(2.0f*dot(n,l)*n - l);
  float spec = (float)specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                material.shininess);
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = diffuse(uv);
  color = c;
  vec3 ambient = material.ambient;
  vec3 emit = emissive(uv);
  
  for (int i=0; i<3; i++) {
    color[i] = emit[i] + std::fmin(ambient[i] + 
      c[i]*shadow*(diff + 
      spec * material.specular_intensity),1);
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  return false;
}

PhongNormalShader::~PhongNormalShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

PhongNormalShader::PhongNormalShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
            vec3 light_dir, rayimage& shadowbuffer,
            Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
            material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M)); 
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
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

vec4 PhongNormalShader::vertex(int iface, int nthvert, ModelInfo& model) {
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
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
    }
  }
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;

  vec3 n = normalize(vec3(uniform_MIT * vec4(normal_uv(uv),0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
  float spec = (float)specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                        material.shininess);
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = diffuse(uv);
  color = c;
  vec3 ambient = material.ambient;
  vec3 emit = emissive(uv);
  
  for (int i=0; i<3; i++) {
    color[i] = emit[i] + std::fmin(ambient[i] + 
      c[i]*shadow*(diff + 
            spec * material.specular_intensity*material.specular[i]),1);
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  return false;
}

PhongShaderTangent::~PhongShaderTangent() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(has_normal_texture) {
    stbi_image_free(normal_texture);
  }
  if(has_specular_texture) {
    stbi_image_free(specular_texture);
  }
  if(has_emissive_texture) {
    stbi_image_free(emissive_texture);
  }
}

PhongShaderTangent::PhongShaderTangent(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir, rayimage& shadowbuffer,
                         Mat uniform_Mshadow_, bool has_shadow_map, float shadow_map_bias,
                         material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(strcmp(material.diffuse_texname.get_cstring(), "")) {
    has_texture = true;
    
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 4);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(strcmp(material.normal_texname.get_cstring(), "")) {
    has_normal_texture = true;
    
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 4);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(strcmp(material.specular_texname.get_cstring(), "")) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 4);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(strcmp(material.emissive_texname.get_cstring(), "")) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 4);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
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

vec4 PhongShaderTangent::vertex(int iface, int nthvert, ModelInfo& model) {
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
      shadow = 0.0f;
      float bias = shadow_map_bias;
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : 0;    
        }    
      }
      shadow /= 25;
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
  vec3 n = normalize(B * normal_uv(uv));
  
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
  float spec = (float)specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                material.shininess);
  float diff = std::fmax(0.f, dot(n,l));
  vec3 c = diffuse(uv);
  vec3 ambient = material.ambient;
  
  vec3 emit = emissive(uv);
  for (int i=0; i<3; i++) {
    color[i] = emit[i] + std::fmin(ambient[i] +
      c[i]*shadow*(diff +
      spec * material.specular_intensity * material.specular[i]),1);
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  return false;
}

DepthShader::~DepthShader() {}

DepthShader::DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir, int mat_info) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  for(int i = 0; i < mat_info; i++ ) {
    std::vector<vec4> temptri(3);
    vec_varying_tri.push_back(temptri);
  }
}

vec4 DepthShader::vertex(int iface, int nthvert, ModelInfo& model) {
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


