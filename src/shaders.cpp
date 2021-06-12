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
                             reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)  {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);

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
}

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
  
  return (clip);
}

bool GouraudShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  vec4 diffuse_color = vec4(material.diffuse,material.dissolve);
  
  vec4 light_color(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 0.0f;
    Float intensity = std::fmax(dot(normal, vec3(uniform_M * vec4(directional_lights[ii].direction,0.0))),0.0);
    if(has_shadow_map && intensity != 0.0) {
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffers[ii].get_shadow_intensity();    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += vec4(vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity,0.0) * shadow * intensity;
  }
  // color = vec4(light_color * material.diffuse,1.0f);
  color = light_color; 
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
                             material_info mat_info,
                             std::vector<vec3>& vec_varying_intensity,
                             std::vector<std::vector<vec3> >& vec_varying_uv,
                             std::vector<std::vector<vec4> >& vec_varying_tri,
                             std::vector<std::vector<vec3> >& vec_varying_pos,
                             std::vector<std::vector<vec3> >& vec_varying_world_nrm,
                             std::vector<std::vector<vec3> >& vec_varying_ndc_tri,
                             std::vector<std::vector<vec3> >& vec_varying_nrm,
                             reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport), material(mat_info),
  vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)   {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);

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
}


vec4 ColorShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec4 clip = MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] =  vp * clip;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  vec_varying_world_nrm[iface][nthvert] = model.model_vertex_normals(iface) ?
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
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
              reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
    Projection(Projection), View(View), viewport(viewport),
    has_shadow_map(has_shadow_map),
    shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
    directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
    vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
    vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
    reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);

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
}


vec4 DiffuseShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec4 clip = MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] =  vp * clip;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  
  vec_varying_world_nrm[iface][nthvert] = model.model_vertex_normals(iface) ?
    uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  
  return (vec_varying_tri[iface][nthvert]);
}

bool DiffuseShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);

  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;

  vec3 light_color(0.0);

  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    Float shadow = 1.0f;
    Float intensity = std::fmax(dot(normal, vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0))),0.0);
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);

        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffers[ii].get_shadow_intensity();    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity +
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
  }


  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  //Directional light contribution
  color = diffuse_color * vec4(light_color,1.0);
  
  
  for(unsigned int i = 0; i < plights.size(); i++) {
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f,dot(normal, plights[i].CalcLightDir(pos)));
  }
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
             reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
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
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
}


vec4 DiffuseNormalShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  
  
  return (clip);
}

bool DiffuseNormalShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  vec3 n = normalize(vec3(uniform_MIT * vec4(normal_uv(uv), 0.0f)));
  
  vec3 light_color(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    Float intensity = std::fmax(dot(n, vec3(uniform_M * vec4(directional_lights[ii].direction,0.0))),0.0);
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffers[ii].get_shadow_intensity();    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
  }
  
  for(unsigned int i = 0; i < plights.size(); i++) {
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f,dot(normal, plights[i].CalcLightDir(pos)));
  }
  
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  normal = n;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
                                       reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), 
  vec_varying_ndc_tri(vec_varying_ndc_tri), vec_varying_world_nrm(vec_varying_world_nrm),
  vec_varying_nrm(vec_varying_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
  
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
  
  
  return clip;
}

bool DiffuseShaderTangent::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  
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
  vec3 norm = normalize(B * normal_uv(uv));
  
  vec3 light_color(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    Float intensity = std::fmax(dot(norm, vec3(uniform_M * vec4(directional_lights[ii].direction,0.0))),0.0);
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffers[ii].get_shadow_intensity();    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec4 dir_shadow_int = vec4(light_color, 1.0f);
  //Directional light contribution
  color = diffuse_color * dir_shadow_int;
  for(unsigned int i = 0; i < plights.size(); i++) {
    vec3 p_light_dir = vec4(plights[i].CalcLightDir(pos),0.0f);
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f, dot(norm, p_light_dir));
  }
  
  
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  normal =  norm;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
                                     reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), 
  vec_varying_nrm(vec_varying_nrm), vec_varying_pos(vec_varying_pos), 
  vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
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
  uniform_MIT = glm::inverse(glm::transpose(uniform_M));
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
}

vec4 PhongShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  
  vec_varying_world_nrm[iface][nthvert] = model.model_vertex_normals(iface) ?
    uniform_MIT * normalize(vec4(model.normal(iface, nthvert),0.0f)) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  
  return clip;
}

bool PhongShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  (vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z);
  
  vec3 spec_uv = specular(uv);
  vec3 light_color(0.0);
  vec4 spec_total(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    vec3 l_dir = vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0));
    Float intensity = std::fmax(dot(normal, l_dir),0.0);
    Float shadow_int = shadowbuffers[ii].get_shadow_intensity();
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0 : shadow_int;    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
    vec3 r = normalize(2.0f*dot(normal,l_dir)*normal - l_dir);
    spec_total += vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f) * (shadow) * directional_lights[ii].intensity; 
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec4 amb = ambient(uv);
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(light_color,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec_total,(Float)0.0,(Float)1.0);
  
  for(unsigned int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    vec3 r = normalize(2.0f*dot(normal,l_p)*normal - l_p);
    vec4 spec = vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),(Float)0.0) * fmax(0.0f, dot(normal, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0,(Float)1.0);
  }
  color += amb;
  color += emit;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
            reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::transpose(glm::inverse(uniform_M)); 
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
  
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
}

vec4 PhongNormalShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = vec3(View * Model * vec4(model.vertex(iface, nthvert),1.0f));
  vec_varying_world_nrm[iface][nthvert] = vec3(uniform_MIT * vec4(model.normal(iface, nthvert),0.0f));
  
  vec3 gl_Vertex = model.vertex(iface,nthvert);
  vec4 clip = vp * MVP * vec4(gl_Vertex, 1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  
  
  return clip;
}

bool PhongNormalShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal = normalize(vec3(uniform_MIT * vec4(normal_uv(uv),0.0f)));
  
  vec3 spec_uv = specular(uv);
  vec3 light_color(0.0);
  vec4 spec_total(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    vec3 l_dir = vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0));
    Float intensity = std::fmax(dot(normal, l_dir),0.0);
    Float shadow_int = shadowbuffers[ii].get_shadow_intensity();
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0 : shadow_int;    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
    vec3 r = normalize(2.0f*dot(normal,l_dir)*normal - l_dir);
    spec_total += vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f) * (shadow) * directional_lights[ii].intensity; 
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec3 ambient = material.ambient;
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(light_color,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec_total,(Float)0.0,(Float)1.0);

  
  for(unsigned int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    vec3 r = normalize(2.0f*dot(normal,l_p)*normal - l_p);
    vec4 spec = vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),(Float)0.0) * fmax(0.0f, dot(normal, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0,(Float)1.0);
  }
  color += vec4(ambient,0.0f);
  color += emit;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
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
                         reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), 
  vec_varying_ndc_tri(vec_varying_ndc_tri), vec_varying_world_nrm(vec_varying_world_nrm),
  vec_varying_nrm(vec_varying_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)     {
  MVP = Projection * View * Model;
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
  
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
  
  
  return clip;
}

bool PhongShaderTangent::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  
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
  normal = normalize(B * normal_uv(uv));
  
  vec3 spec_uv = specular(uv);
  vec3 light_color(0.0);
  vec4 spec_total(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    vec3 l_dir = vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0));
    Float intensity = std::fmax(dot(normal, l_dir),0.0);
    Float shadow_int = shadowbuffers[ii].get_shadow_intensity();
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0 : shadow_int;    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w * directional_lights[ii].intensity + 
      (1-trans_color.w) *  directional_lights[ii].color * directional_lights[ii].intensity) * shadow * intensity;
    vec3 r = normalize(2.0f*dot(normal,l_dir)*normal - l_dir);
    spec_total += vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f) * (shadow) * directional_lights[ii].intensity; 
  }
  
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec4 amb = ambient(uv);
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(light_color,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec_total,(Float)0.0,(Float)1.0);
  

  for(unsigned int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    vec3 r = normalize(2.0f*dot(normal,l_p)*normal - l_p);
    vec4 spec = vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),(Float)0.0) * fmax(0.0f, dot(normal, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0,(Float)1.0);
  }
  color += amb;
  color += emit;
  if(has_reflection && !has_refraction) {
    vec3 dir = uniform_M_inv * vec4(glm::normalize(pos),0.0f);
    vec3 normal_w =  uniform_MIT_inv * vec4(glm::normalize(normal),0.0f);
    vec3 r = glm::reflect(dir,normal_w);
    color = material.reflection_intensity * reflection(r) + (1-material.reflection_intensity) * color;
  } 
  if(has_refraction) {
    vec4 temp_col = diffuse_color;
    vec3 dir = glm::normalize(uniform_M_inv * vec4(pos,0.0f));
    vec3 normal_w =  glm::normalize(uniform_MIT_inv * vec4(normal,0.0f));
    vec3 rl = glm::reflect(dir,normal_w);
    
    vec3 rf = glm::refract(dir,normal_w, 1.0/material.ior);
    vec4 reflect_col = reflection(rl);
    vec4 refract_col = reflection(rf);
    Float Ro = (material.ior - 1.0)/(material.ior + 1.0);
    Ro = Ro * Ro;
    Float c0 = 1.0-dot(dir,-normal_w);
    Float c0_5 = c0*c0*c0*c0*c0;
    Float reflect_coef = Ro + (1.0 - Ro) * c0_5;
    color = (1-reflect_coef) * refract_col * temp_col +  reflect_coef * reflect_col;
    color.w = 1.0;
  }
  return false;
}

DepthShader::~DepthShader() {
  if(material.has_texture) {
    stbi_image_free(texture);
  }
}

DepthShader::DepthShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                          material_info mat_info, int mat_ind,
                          std::vector<std::vector<vec3> >& vec_varying_uv,
                          std::vector<std::vector<vec4> >& vec_varying_tri
                          ) :
  Projection(Projection), View(View), viewport(viewport),
  material(mat_info), vec_varying_uv(vec_varying_uv), vec_varying_tri(vec_varying_tri)
  {
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
  color = diffuse_color;
  pos = p;
  return false;
}


ToonShader::ToonShader(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                             
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
                             reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
  
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
}

ToonShader::~ToonShader() {
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


vec4 ToonShader::vertex(int iface, int nthvert, ModelInfo& model) {
  vec4 clip = MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_tri[iface][nthvert] =  vp * clip;
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  
  vec_varying_world_nrm[iface][nthvert] = model.model_vertex_normals(iface) ?
  uniform_MIT * vec4(model.normal(iface, nthvert),0.0f) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  
  
  return (vec_varying_tri[iface][nthvert]);
}

bool ToonShader::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z;
  
  vec3 light_color(0.0);
  
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    Float shadow = 1.0f;
    Float intensity = std::fmax(dot(normal, vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0))),0.0);
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0f : shadowbuffers[ii].get_shadow_intensity();    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w + 
      (1-trans_color.w) *  directional_lights[ii].color) * 
      (Float)round(material.toon_levels * directional_lights[ii].intensity * shadow * intensity)/material.toon_levels;
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  //Directional light contribution
  color = diffuse_color * vec4(light_color,1.0);
  
  
  for(unsigned int i = 0; i < plights.size(); i++) {
    color += diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),0.0f) * fmax(0.0f,dot(normal, plights[i].CalcLightDir(pos)));
  }
  //Emissive and ambient terms
  color += emissive(uv);
  color += ambient(uv);
  return false; 
}

ToonShaderPhong::ToonShaderPhong(Mat& Model, Mat& Projection, Mat& View, vec4& viewport,
                       
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
                       reflection_map_info reflection_map, bool has_reflection, bool has_refraction) :
  Projection(Projection), View(View), viewport(viewport),
  has_shadow_map(has_shadow_map),
  shadow_map_bias(shadow_map_bias), material(mat_info), plights(point_lights), 
  directional_lights(directional_lights), shadowbuffers(shadowbuffers), transparency_buffers(transparency_buffers),
  vec_varying_intensity(vec_varying_intensity), vec_varying_uv(vec_varying_uv),
  vec_varying_tri(vec_varying_tri), vec_varying_pos(vec_varying_pos), vec_varying_world_nrm(vec_varying_world_nrm),
  reflection_map(reflection_map), has_reflection(has_reflection), has_refraction(has_refraction)    {
  vp = glm::scale(glm::translate(Mat(1.0f),
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f)), 
                                 vec3(viewport[2]/2.0f,viewport[3]/2.0f,1.0f/2.0f));
  MVP = Projection * View * Model;
  uniform_M = View * Model;
  uniform_MIT = glm::inverseTranspose(uniform_M);
  uniform_M_inv = glm::inverse(uniform_M);
  uniform_MIT_inv = glm::inverse(uniform_MIT);
  
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
}

ToonShaderPhong::~ToonShaderPhong() {
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


vec4 ToonShaderPhong::vertex(int iface, int nthvert, ModelInfo& model) {
  vec_varying_uv[iface][nthvert] = model.tex(iface,nthvert);
  vec_varying_pos[iface][nthvert] = uniform_M * vec4(model.vertex(iface, nthvert),1.0f);
  vec4 clip = vp * MVP * vec4(model.vertex(iface,nthvert),1.0f);
  vec_varying_tri[iface][nthvert] = clip;
  
  vec_varying_world_nrm[iface][nthvert] = model.model_vertex_normals(iface) ?
  uniform_MIT * normalize(vec4(model.normal(iface, nthvert),0.0f)) : 
    uniform_MIT * normalize(vec4(glm::cross(model.vertex(iface,1)-model.vertex(iface,0),
                                            model.vertex(iface,2)-model.vertex(iface,0)),0.0f));
  
  return clip;
}

bool ToonShaderPhong::fragment(const vec3& bc, vec4 &color, vec3& pos, vec3& normal, int iface) {
  vec3 uv = vec_varying_uv[iface][0] * bc.x + vec_varying_uv[iface][1] * bc.y + vec_varying_uv[iface][2] * bc.z;
  vec4 diffuse_color = diffuse(uv);
  if(diffuse_color.w == 0.0) return true;
  normal =  (vec_varying_world_nrm[iface][0] * bc.x + vec_varying_world_nrm[iface][1] * bc.y + vec_varying_world_nrm[iface][2] * bc.z);
  
  vec3 spec_uv = specular(uv);
  vec3 light_color(0.0);
  vec4 spec_total(0.0);
  for(unsigned int ii = 0; ii < directional_lights.size(); ii++) {
    vec4 trans_color(0.0,0.0,0.0,0.0);
    
    Float shadow = 1.0f;
    vec3 l_dir = vec3(uniform_M * vec4(directional_lights[ii].direction, 0.0));
    Float intensity = std::fmax(dot(normal, l_dir),0.0);
    Float shadow_int = shadowbuffers[ii].get_shadow_intensity();
    if(has_shadow_map && intensity != 0.0) {
      shadow = 0.0f;
      
      vec4 sb_p = directional_lights[ii].uniform_Mshadow_ * (vec_varying_tri[iface][0] * bc.x + vec_varying_tri[iface][1] * bc.y + vec_varying_tri[iface][2] * bc.z);
      sb_p = sb_p/sb_p.w;
      if(sb_p[0] >= 0 && sb_p[0] < shadowbuffers[ii].width() && sb_p[1] >= 0 && sb_p[1] < shadowbuffers[ii].height()) {
        Float bias = std::fmax(shadow_map_bias*10.0 * (1.0 - intensity),shadow_map_bias);
        
        int i = int(sb_p[0]);
        int j = int(sb_p[1]);
        for(int x = -2; x <= 2; ++x) {
          for(int y = -2; y <= 2; ++y) {
            shadow += shadowbuffers[ii].get_color_bounded(i+x,j+y).x > sb_p[2]-bias ? 1.0 : shadow_int;    
          }    
        }
        shadow /= 25;
        if(diffuse_color.w == 1.0) {
          for(int x = -2; x <= 2; ++x) {
            for(int y = -2; y <= 2; ++y) {
              trans_color += transparency_buffers[ii].get_color_bounded_a(i+x,j+y);
            }
          }
          trans_color /= 25;
        }
      }
    }
    light_color += (vec3(trans_color) * trans_color.w + 
      (1-trans_color.w) *  directional_lights[ii].color) * 
      (Float)round(material.toon_levels * directional_lights[ii].intensity * shadow * intensity)/material.toon_levels;
    vec3 r = normalize(2.0f*dot(normal,l_dir)*normal - l_dir);
    spec_total += vec4(spec_uv * 
    (Float)round(material.toon_levels * std::pow(std::fmax(r.z, 0.0f), material.shininess) * shadow * directional_lights[ii].intensity)/material.toon_levels,0.0f); 
  }
  pos =  vec_varying_pos[iface][0] * bc.x + vec_varying_pos[iface][1] * bc.y + vec_varying_pos[iface][2] * bc.z;
  
  vec4 amb = ambient(uv);
  vec4 emit = emissive(uv);
  vec4 shadow_vec = vec4(light_color,1.0f);
  color = clamp( diffuse_color*shadow_vec + spec_total,(Float)0.0,(Float)1.0);
  
  for(unsigned int i = 0; i < plights.size(); i++) {
    vec3 l_p = vec4(plights[i].CalcLightDir(pos),0.0f);
    vec3 r = normalize(2.0f*dot(normal,l_p)*normal - l_p);
    vec4 spec = vec4(spec_uv * std::pow(std::fmax(r.z, 0.0f), material.shininess),0.0f);
    color += clamp(diffuse_color * vec4(plights[i].CalcPointLightAtten(pos),(Float)0.0) * fmax(0.0f, dot(normal, l_p)) +
      vec4(plights[i].CalcPointLightAtten(pos),0.0f) * spec, (Float)0.0,(Float)1.0);
  }
  color += amb;
  color += emit;
  
  return false;
}


