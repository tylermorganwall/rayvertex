#include "shaders.h"

#include "RcppThread.h"
#include "stb_image.h"


void print_vec(vec3 m) {
  RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << "\n";
}

void print_vec(vec4 m) {
  RcppThread::Rcout << std::fixed << m[0] << " " << m[1] << " " << m[2] << " " << m[3] << "\n";
}

inline Float clamp(Float c, Float clamplow, Float clamphigh) {
  if(c > clamphigh) {
    return(clamphigh);
  } else if(c < clamplow) {
    return(clamplow);
  }
  return(c);
}

IShader::~IShader() {}

GouraudShader::GouraudShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             vec3 light_dir, rayimage& shadowbuffer,
                             Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
                             material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity,
                             std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));

  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
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
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
  
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  return (clip);
}

bool GouraudShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec3 n = normalize(vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z);
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(n, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1 : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  color = vec4(shadow * material.diffuse * dot(vec_varying_intensity[iface],bc),1.0f);
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;

  return(false);
}


ColorShader::~ColorShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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

ColorShader::ColorShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             material_info mat_info) :
  Projection(Projection), View(View), viewport(viewport), material(mat_info) {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);

  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
    std::vector<vec3> temppos(3);
    std::vector<vec3> tempnrm(3);
    
    vec_varying_uv.push_back(tempuv);
    vec_varying_tri.push_back(temptri);
    vec_varying_pos.push_back(temppos);
    vec_varying_world_nrm.push_back(tempnrm);
  }
};


vec4 ColorShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec4 clip = MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] =  vp * clip;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  vec_varying_world_nrm[iface][nthvert] = model.has_normals ?
    uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  return (vec_varying_tri[iface][nthvert]);
}

bool ColorShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  
  color = diffuse_color;
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  return false; 
}

DiffuseShader::~DiffuseShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
              Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
              material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity, 
              std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)  {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_Mshadow = uniform_Mshadow_;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
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
  vec4 clip = MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] =  vp * clip;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  
  vec_varying_world_nrm[iface][nthvert] = model.has_normals ?
    uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
    
  vec_varying_intensity[iface][nthvert] = std::fmax(0.f, dot(vec_varying_world_nrm[iface][nthvert], l));
  has_normals = model.has_normals;
  return (vec_varying_tri[iface][nthvert]);
}

bool DiffuseShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(normal, l)),shadow_map_bias);
      
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow += shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  Float intensity = dot(vec_varying_intensity[iface],bc) * dirlightintensity;
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;

  vec4 dir_shadow_int = vec4(intensity * shadow, intensity * shadow, intensity * shadow, 1.0f);
  //Directional light contribution
  color = diffuse_color * dir_shadow_int;
  
  for(int i = 0; i < plights.size(); i++) {
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f,dot(normal, plights[i].CalcLightDir(pos)));
  }

  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  
  return false; 
}

DiffuseNormalShader::~DiffuseNormalShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
             Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
             material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity, 
             std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)  {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
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
    std::vector<vec4> temptri(3);
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
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  
  return (clip);
}

bool DiffuseNormalShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  vec3 n = normalize(vec3(uniform_MIT * vec4(normal_uv(uv), 0.0f)));
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(n, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  Float intensity = std::fmax(0.f, dot(n,l));
  
  vec4 dir_shadow_int = vec4(intensity * shadow, intensity * shadow, intensity * shadow, 1.0f);
  //Directional light contribution
  color = diffuse_color * dir_shadow_int;
  
  for(int i = 0; i < plights.size(); i++) {
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f,dot(normal, plights[i].CalcLightDir(pos)));
  }
  
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;
  normal = n;
  return false;
}

DiffuseShaderTangent::~DiffuseShaderTangent() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
                                       Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
                                       material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity,
                                       std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)  {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
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
  vec_varying_pos[iface][nthvert] = vec3(uniform_M * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);

  vec4 ndc = MVP  * vec4(gl_Vertex, 1.0f);
  vec_varying_ndc_tri[iface][nthvert] = vec3(ndc/ndc.w);
  vec4 clip = vp * MVP * vec4(gl_Vertex, 1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  
  return clip;
}

bool DiffuseShaderTangent::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  vec3 norm = vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(norm, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow += shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec3 bn = normalize(vec_varying_nrm[iface][0] * bc.x + 
    vec_varying_nrm[iface][1] * bc.y + 
    vec_varying_nrm[iface][2] * bc.z);

  glm::mat3 A{(vec_varying_ndc_tri[iface][1] - vec_varying_ndc_tri[iface][0]),
              (vec_varying_ndc_tri[iface][2] - vec_varying_ndc_tri[iface][0]),bn};
  glm::mat3 AI = inverse(transpose(A));
  vec3 i = AI * vec3(vec_varying_uv[iface][1].x - vec_varying_uv[iface][0].x, 
                     vec_varying_uv[iface][2].x - vec_varying_uv[iface][0].x, 0.0f);
  vec3 j = AI * vec3(vec_varying_uv[iface][1].y - vec_varying_uv[iface][0].y, 
                     vec_varying_uv[iface][2].y - vec_varying_uv[iface][0].y, 0.0f);
  glm::mat3 B = (glm::mat3{ normalize(i), normalize(j), bn });
  vec3 n = normalize(B * normal_uv(uv));
  
  Float diff = std::fmax(0.f, dot(n,l));
  
  vec4 dir_shadow_int = vec4(diff * shadow, diff * shadow, diff * shadow, 1.0f);
  //Directional light contribution
  color = diffuse_color * dir_shadow_int;
  for(int i = 0; i < plights.size(); i++) {
    vec3 p_light_dir = vec4(plights[i].CalcLightDir(pos),0.0f);
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f, dot(n, p_light_dir));
  }
  
  
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  normal =  n;
  
  return false;
}

PhongShader::~PhongShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
                                     Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
                                     material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity,
                                     std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)  {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
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
    std::vector<vec4> temptri(3);
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
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  vec_varying_world_nrm[iface][nthvert] = has_normals ?
  uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  
  return clip;
}

bool PhongShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(normal, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f: shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;

  vec3 n = has_normals ?  
    normalize(vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z) :
    normalize(glm::cross(vec3(vec_varying_tri[iface][1]-vec_varying_tri[iface][0]),vec3(vec_varying_tri[iface][2]-vec_varying_tri[iface][0])));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
  vec4 spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f); 
  
  Float diff = std::fmax(0.f, dot(n,l));
  vec3 ambient = material.ambient;
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(shadow*diff,shadow*diff,shadow*diff,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec,(Float)0.0,(Float)1.0);
  
  for(int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    r = normalize(2.0f*dot(n,l_p)*n - l_p);
    spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),(Float)0.0) * fmax(0.0f, dot(n, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0,(Float)1.0);
  }
  color += vec4(ambient,0.0f);
  color += emit;
  
  return false;
}

PhongNormalShader::~PhongNormalShader() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
            Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
            material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity,
            std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)  {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M)); 
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
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
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  
  return clip;
}

bool PhongNormalShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(normal, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;

  vec3 n = normalize(vec3(uniform_MIT * vec4(normal_uv(uv),0.0f)));
  vec3 r = normalize(2.0f*dot(n,l)*n - l);
  vec4 spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f),
                        material.shininess),0.0f);
  Float diff = std::fmax(0.f, dot(n,l));
  vec3 ambient = material.ambient;
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(shadow*diff,shadow*diff,shadow*diff,(Float)1.0);
  color = clamp( diffuse_color*shadow_vec + spec,(Float)0.0,(Float)1.0);
  
  for(int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    r = normalize(2.0f*dot(n,l_p)*n - l_p);
    spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f, dot(n, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0f,(Float)1.0f);
  }
  color += vec4(ambient,0.0f);
  color += emit;

  return false;
}

PhongShaderTangent::~PhongShaderTangent() {
  if(has_texture) {
    stbi_image_free(texture);
  }
  if(material.has_ambient_texture) {
    stbi_image_free(ambient_texture);
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
                         Mat uniform_Mshadow_, bool has_shadow_map, Float shadow_map_bias,
                         material_info mat_info,  std::vector<Light>& point_lights, Float lightintensity,
                         std::vector<DirectionalLight>& directional_lights) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir), shadowbuffer(shadowbuffer), has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), dirlightintensity(lightintensity),
  directional_lights(directional_lights)   {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_Mshadow = uniform_Mshadow_;
  l = normalize(vec3(uniform_M * vec4(light_dir, 0.0f)));
  
  has_texture = has_normal_texture = has_specular_texture = has_emissive_texture = false;
  if(material.has_texture) {
    has_texture = true;
    texture = stbi_loadf(material.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  if(material.has_ambient_texture) {
    ambient_texture = stbi_loadf(material.ambient_texname.get_cstring(), &nx_a, &ny_a, &nn_a, 0);
    if(nx_a == 0 || ny_a == 0 || nn_a == 0) {
      throw std::runtime_error("Ambient Texture loading failed");
    }
  }
  if(material.has_normal_texture) {
    has_normal_texture = true;
    normal_texture = stbi_loadf(material.normal_texname.get_cstring(), &nx_nt, &ny_nt, &nn_nt, 0);
    if(nx_nt == 0 || ny_nt == 0 || nn_nt == 0) {
      throw std::runtime_error("Normal texture loading failed");
    }
  }
  if(material.has_specular_texture) {
    has_specular_texture = true;
    specular_texture = stbi_loadf(material.specular_texname.get_cstring(), 
                                  &nx_st, &ny_st, &nn_st, 0);
    if(nx_st == 0 || ny_st == 0 || nn_st == 0) {
      throw std::runtime_error("Specular texture loading failed");
    }
  }
  if(material.has_emissive_texture) {
    has_emissive_texture = true;
    emissive_texture = stbi_loadf(material.emissive_texname.get_cstring(), 
                                  &nx_et, &ny_et, &nn_et, 0);
    if(nx_et == 0 || ny_et == 0 || nn_et == 0) {
      throw std::runtime_error("Emissive texture loading failed");
    }
  }
  
  
  for(int i = 0; i < material.max_indices; i++ ) {
    std::vector<vec3> tempuv(3);
    std::vector<vec4> temptri(3);
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
  vec_varying_pos[iface][nthvert] = vec3(uniform_M * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = MVP * vec4(gl_Vertex,1.0f);
  vec_varying_ndc_tri[iface][nthvert] = clip/clip.w;
  clip = vp * clip;
  vec_varying_tri[iface][nthvert] = clip;
  has_normals = model.has_normals;
  
  return clip;
}

bool PhongShaderTangent::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;;
  
  Float shadow = 1.0f;
  if(has_shadow_map) {
    vec4 sb_p = uniform_Mshadow * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
    sb_p = sb_p/sb_p.w;
    if(sb_p[0] >= 0 && sb_p[0] < shadowbuffer.width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffer.height()) {
      shadow = 0.0f;
      Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - dot(normal, l)),shadow_map_bias);
      int i = int(sb_p[0]);
      int j = int(sb_p[1]);
      for(int x = -2; x <= 2; ++x) {
        for(int y = -2; y <= 2; ++y) {
          shadow +=shadowbuffer.get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffer.get_shadow_intensity();    
        }    
      }
      shadow /= 25;
    }
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;;

  vec3 bn = (vec_varying_nrm[iface][0] * bc.x + 
    vec_varying_nrm[iface][1] * bc.y + 
    vec_varying_nrm[iface][2] * bc.z);

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
  vec4 spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
  Float diff = std::fmax(0.f, dot(n,l));
  vec4 c = diffuse_color;
  vec4 ambient = vec4(material.ambient,0.0f);
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(shadow*diff,shadow*diff,shadow*diff,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec,(Float)0.0f,(Float)1.0f);

  for(int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    r = normalize(2.0f*dot(n,l_p)*n - l_p);
    spec = vec4(specular(uv) * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f, dot(n, l_p)) +
                   vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0f,(Float)1.0f);
  }
  color += ambient;
  color += emit;
  
  return false;
}

DepthShader::~DepthShader() {
  if(material.has_texture) {
    stbi_image_free(texture);
  }
}

DepthShader::DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                         vec3 light_dir,  material_info mat_info, int mat_ind) :
  Projection(Projection), View(View), viewport(viewport),
  light_dir(light_dir),  material(mat_info) {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                  vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                  vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  has_texture = false;
  if(mat_info.has_texture) {
    has_texture = true;
    texture = stbi_loadf(mat_info.diffuse_texname.get_cstring(), &nx_t, &ny_t, &nn_t, 0);
    if(nx_t == 0 || ny_t == 0 || nn_t == 0) {
      throw std::runtime_error("Texture loading failed");
    }
  }
  for(int i = 0; i < mat_ind; i++ ) {
    std::vector<vec4> temptri(3);
    std::vector<vec3> tempuv(3);
    
    vec_varying_tri.push_back(temptri);
    vec_varying_uv.push_back(tempuv);
  }
}

vec4 DepthShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex,1.0);
  vec_varying_tri[iface][nthvert] = clip;
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  
  return  clip;
}

bool DepthShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  
  vec4 p = vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z;
  color = vec4(p.z,p.z,p.z,diffuse_color.w);
  return false;
}


