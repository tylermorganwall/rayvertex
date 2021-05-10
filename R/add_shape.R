#'@title Add Shape
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
add_shape = function(mesh, shape) {
  if(length(mesh) == 0) {
    return(shape)
  }
  max_vertices = nrow(mesh$vertices)
  max_norms = nrow(mesh$normals)
  max_tex = nrow(mesh$texcoords)
  max_material = length(mesh$materials)
  
  for(i in seq_len(length(shape$shapes))) {
    shape$shapes[[i]]$indices      = shape$shapes[[i]]$indices      + max_vertices
    shape$shapes[[i]]$tex_indices  = shape$shapes[[i]]$tex_indices  + max_tex
    shape$shapes[[i]]$norm_indices = shape$shapes[[i]]$norm_indices + max_norms
    shape$shapes[[i]]$material_ids = ifelse(shape$shapes[[i]]$material_ids != -1, 
                                            shape$shapes[[i]]$material_ids + max_material,
                                            -1)
  }
  mesh$shapes = c(mesh$shapes,shape$shapes)

  mesh$vertices  = rbind(mesh$vertices,  shape$vertices)
  mesh$normals   = rbind(mesh$normals,   shape$normals)
  mesh$texcoords = rbind(mesh$texcoords, shape$texcoords)
  mesh$materials = c(mesh$materials,shape$materials)
  if(!is.null(attr(shape,"cornell")) || !is.null(attr(mesh,"cornell"))) {
    attr(mesh,"cornell") = TRUE
    if(!is.null(attr(shape,"cornell"))) {
      attr(shape,"cornell_light") = attr(shape,"cornell_light")
      attr(scene,"cornell_diffuse_light") = attr(shape,"cornell_diffuse_light")
    } else {
      attr(mesh,"cornell_light") = attr(mesh,"cornell_light")
      attr(mesh,"cornell_diffuse_light") = attr(mesh,"cornell_diffuse_light")
    }
  }
  return(mesh)
}

#'@title Add Shape
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
merge_shapes = function(mesh) {
  indices = list()
  tex_indices = list()
  norm_indices = list()
  material_ids = list()
  
  for(i in seq_len(length(mesh$shapes))) {
    indices[[i]]      = mesh$shapes[[i]]$indices     
    tex_indices[[i]]  = mesh$shapes[[i]]$tex_indices 
    norm_indices[[i]] = mesh$shapes[[i]]$norm_indices
    material_ids[[i]] = mesh$shapes[[i]]$material_ids
  }
  
  mesh$shapes = list()
  mesh$shapes[[1]] = list()
  
  mesh$shapes[[1]]$indices = do.call(rbind,indices)
  mesh$shapes[[1]]$tex_indices = do.call(rbind,tex_indices)
  mesh$shapes[[1]]$norm_indices = do.call(rbind,norm_indices)
  mesh$shapes[[1]]$material_ids = unlist(material_ids)
  
  return(mesh)
}

#'@title Add Shape
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
translate_mesh = function(mesh, position = c(0,0,0)) {
  mesh$vertices[,1]  = mesh$vertices[,1] + position[1]
  mesh$vertices[,2]  = mesh$vertices[,2] + position[2]
  mesh$vertices[,3]  = mesh$vertices[,3] + position[3]
  return(mesh)
}

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
scale_mesh = function(mesh, scale = 1, center = c(0,0,0)) {
  if(length(scale) == 1) {
    scale = rep(scale,3)
  }
  mesh$vertices[,1]  = (mesh$vertices[,1]-center[1])*scale[1] + center[1]
  mesh$vertices[,2]  = (mesh$vertices[,2]-center[2])*scale[2] + center[2]
  mesh$vertices[,3]  = (mesh$vertices[,3]-center[3])*scale[3] + center[3]
  
  if(!is.null(mesh$normals) && nrow(mesh$normals) > 0) {
    mesh$normals[,1]  = mesh$normals[,1]*1/scale[1]
    mesh$normals[,2]  = mesh$normals[,2]*1/scale[2]
    mesh$normals[,3]  = mesh$normals[,3]*1/scale[3]
    for(i in seq_len(nrow(mesh$normals))) {
      length_single = sqrt(mesh$normals[i,1]^2 + mesh$normals[i,2]^2 + mesh$normals[i,3]^2)
      mesh$normals[i,] = mesh$normals[i,]/length_single
    }
  }
  return(mesh)
}

#'@return Matrix
#'@keywords internal
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
generate_rot_matrix = function(angle, order_rotation) {
  rots = list()
  rots[[1]] = matrix(c(1,0,0,0,cos(angle[1]),sin(angle[1]),0,-sin(angle[1]),cos(angle[1])),3,3)
  rots[[2]] = matrix(c(cos(angle[2]),0,-sin(angle[2]),0,1,0,sin(angle[2]),0,cos(angle[2])),3,3)
  rots[[3]] = matrix(c(cos(angle[3]),sin(angle[3]),0,-sin(angle[3]),cos(angle[3]),0,0,0,1),3,3)
  returnmat = matrix(c(1,0,0,0,1,0,0,0,1),3,3)
  for(i in 1:3) {
    returnmat = returnmat %*% rots[[order_rotation[i]]]
  }
  return(returnmat)
}

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
rotate_mesh = function(mesh, angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  angle = angle*pi/180
  mesh$vertices[,1]  = mesh$vertices[,1]-pivot_point[1]
  mesh$vertices[,2]  = mesh$vertices[,2]-pivot_point[2]
  mesh$vertices[,3]  = mesh$vertices[,3]-pivot_point[3]
  rot_mat = generate_rot_matrix(angle, order_rotation)
  for(i in seq_len(nrow(mesh$vertices))) {
    mesh$vertices[i,] = mesh$vertices[i,] %*% rot_mat
  }
  mesh$vertices[,1]  = mesh$vertices[,1]+pivot_point[1]
  mesh$vertices[,2]  = mesh$vertices[,2]+pivot_point[2]
  mesh$vertices[,3]  = mesh$vertices[,3]+pivot_point[3]

  if(!is.null(mesh$normals) && nrow(mesh$normals) > 0) {
    inv_t = t(solve(rot_mat))
    for(i in seq_len(nrow(mesh$normals))) {
      mesh$normals[i,] = mesh$normals[i,] %*% inv_t
    }
  }
  return(mesh)
}

#'@title Set Material
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
set_material = function(mesh, material = NULL,
                        diffuse                   = c(0.5,0.5,0.5),
                        ambient                   = c(0,0,0),
                        specular                  = c(1,1,1),
                        transmittance             = c(1,1,1),
                        emission                  = c(0,0,0),
                        shininess                 = 10.0,
                        ior                       = 1.0,
                        dissolve                  = 1.0,
                        illum                     = 1.0,
                        texture_location          = "",
                        normal_texture_location   = "",
                        specular_texture_location = "",
                        ambient_texture_location  = "",
                        emissive_texture_location = "",
                        diffuse_intensity         = 1, 
                        specular_intensity        = 1,  
                        emission_intensity        = 1,
                        culling                   = "back",
                        type                      = "diffuse") {
  culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  
  if(!is.null(material)) {
    diffuse                   = material$diffuse                   
    ambient                   = material$ambient                   
    specular                  = material$specular                  
    transmittance             = material$transmittance             
    emission                  = material$emission                  
    shininess                 = material$shininess                 
    ior                       = material$ior                       
    dissolve                  = material$dissolve                  
    illum                     = material$illum                     
    texture_location          = material$texture_location          
    normal_texture_location   = material$normal_texture_location   
    specular_texture_location = material$specular_texture_location 
    ambient_texture_location  = material$ambient_texture_location  
    emissive_texture_location = material$emissive_texture_location 
    diffuse_intensity         = material$diffuse_intensity         
    specular_intensity        = material$specular_intensity        
    emission_intensity        = material$emission_intensity        
    culling                   = material$culling                   
    type                      = material$type                      
  }
  
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    for(i in seq_len(length(mesh$materials))) {
      mesh$materials[[i]] = list()
      mesh$materials[[i]]$ambient            = convert_color(ambient)
      mesh$materials[[i]]$diffuse            = convert_color(diffuse)
      mesh$materials[[i]]$specular           = convert_color(specular) 
      mesh$materials[[i]]$transmittance      = convert_color(transmittance)
      mesh$materials[[i]]$emission           = convert_color(emission) 
      mesh$materials[[i]]$shininess          = shininess
      mesh$materials[[i]]$ior                = ior              
      mesh$materials[[i]]$dissolve           = dissolve         
      mesh$materials[[i]]$illum              = illum            
      mesh$materials[[i]]$ambient_texname    = ambient_texture_location  
      mesh$materials[[i]]$diffuse_texname    = texture_location  
      mesh$materials[[i]]$emissive_texname   = emissive_texture_location 
      mesh$materials[[i]]$specular_texname   = specular_texture_location 
      mesh$materials[[i]]$normal_texname     = normal_texture_location   
      mesh$materials[[i]]$diffuse_intensity  = diffuse_intensity 
      mesh$materials[[i]]$specular_intensity = specular_intensity   
      mesh$materials[[i]]$emission_intensity = emission_intensity  
      mesh$materials[[i]]$culling            = culling   
      mesh$materials[[i]]$type               = type   
      
    }
    for(i in seq_len(length(mesh$shapes))) {
      mesh$shapes[[i]]$material_ids = rep(0,nrow(mesh$shapes[[i]]$indices))
    }
  } else {
    mesh$shapes[[1]]$material_ids = rep(0,nrow(mesh$shapes[[1]]$indices))
    
    mesh$materials[[1]] = list()
    mesh$materials[[1]]$ambient            = convert_color(ambient)
    mesh$materials[[1]]$diffuse            = convert_color(diffuse) 
    mesh$materials[[1]]$specular           = convert_color(specular) 
    mesh$materials[[1]]$transmittance      = convert_color(transmittance)
    mesh$materials[[1]]$emission           = convert_color(emission) 
    mesh$materials[[1]]$shininess          = shininess
    mesh$materials[[1]]$ior                = ior              
    mesh$materials[[1]]$dissolve           = dissolve         
    mesh$materials[[1]]$illum              = illum            
    mesh$materials[[1]]$ambient_texname    = ambient_texture_location  
    mesh$materials[[1]]$diffuse_texname    = texture_location  
    mesh$materials[[1]]$emissive_texname   = emissive_texture_location 
    mesh$materials[[1]]$specular_texname   = specular_texture_location 
    mesh$materials[[1]]$normal_texname     = normal_texture_location   
    mesh$materials[[1]]$diffuse_intensity  = diffuse_intensity 
    mesh$materials[[1]]$specular_intensity = specular_intensity   
    mesh$materials[[1]]$emission_intensity = emission_intensity  
    mesh$materials[[1]]$culling            = culling    
    mesh$materials[[1]]$type               = type    
    
  }
  return(mesh)
}

#'@title Add Shape
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
add_shape = function(mesh, shape) {
  if(length(mesh) == 0) {
    return(shape)
  }
  max_vertices = nrow(mesh$vertices)
  max_norms = nrow(mesh$normals)
  max_tex = nrow(mesh$texcoords)
  max_material = length(mesh$materials)
  
  for(i in seq_len(length(shape$shapes))) {
    shape$shapes[[i]]$indices      = shape$shapes[[i]]$indices      + max_vertices
    shape$shapes[[i]]$tex_indices  = shape$shapes[[i]]$tex_indices  + max_tex
    shape$shapes[[i]]$norm_indices = shape$shapes[[i]]$norm_indices + max_norms
    shape$shapes[[i]]$material_ids = ifelse(shape$shapes[[i]]$material_ids != -1, 
                                            shape$shapes[[i]]$material_ids + max_material,
                                            -1)
  }
  mesh$shapes = c(mesh$shapes,shape$shapes)
  
  mesh$vertices  = rbind(mesh$vertices,  shape$vertices)
  mesh$normals   = rbind(mesh$normals,   shape$normals)
  mesh$texcoords = rbind(mesh$texcoords, shape$texcoords)
  mesh$materials = c(mesh$materials,shape$materials)
  return(mesh)
}


#'@return Matrix
#'@keywords internal
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
generate_rot_matrix = function(angle, order_rotation) {
  rots = list()
  rots[[1]] = matrix(c(1,0,0,0,cos(angle[1]),sin(angle[1]),0,-sin(angle[1]),cos(angle[1])),3,3)
  rots[[2]] = matrix(c(cos(angle[2]),0,-sin(angle[2]),0,1,0,sin(angle[2]),0,cos(angle[2])),3,3)
  rots[[3]] = matrix(c(cos(angle[3]),sin(angle[3]),0,-sin(angle[3]),cos(angle[3]),0,0,0,1),3,3)
  returnmat = matrix(c(1,0,0,0,1,0,0,0,1),3,3)
  for(i in 1:3) {
    returnmat = returnmat %*% rots[[order_rotation[i]]]
  }
  return(returnmat)
}

#'@title Change Material
#'
#'Add shape to a mesh
#'
#'@param obj_model  
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
change_material = function(mesh,
                           diffuse                   = NULL,
                           ambient                   = NULL,
                           specular                  = NULL,
                           transmittance             = NULL,
                           emission                  = NULL,
                           shininess                 = NULL,
                           ior                       = NULL,
                           dissolve                  = NULL,
                           illum                     = NULL,
                           texture_location          = NULL,
                           normal_texture_location   = NULL,
                           specular_texture_location = NULL,
                           ambient_texture_location  = NULL,
                           emissive_texture_location = NULL,
                           diffuse_intensity         = NULL,
                           specular_intensity        = NULL,
                           emission_intensity        = NULL,
                           culling                   = NULL,
                           type                      = NULL) {
  if(!is.null(culling)) {
    culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  }
  
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    for(i in seq_len(length(mesh$materials))) {
      if(!is.null(ambient))                   mesh$materials[[i]]$ambient            = convert_color(ambient)
      if(!is.null(diffuse))                   mesh$materials[[i]]$diffuse            = convert_color(diffuse)
      if(!is.null(specular))                  mesh$materials[[i]]$specular           = convert_color(specular)
      if(!is.null(transmittance))             mesh$materials[[i]]$transmittance      = convert_color(transmittance)
      if(!is.null(emission))                  mesh$materials[[i]]$emission           = convert_color(emission)
      if(!is.null(shininess))                 mesh$materials[[i]]$shininess          = shininess
      if(!is.null(ior))                       mesh$materials[[i]]$ior                = ior              
      if(!is.null(dissolve))                  mesh$materials[[i]]$dissolve           = dissolve         
      if(!is.null(illum))                     mesh$materials[[i]]$illum              = illum            
      if(!is.null(ambient_texture_location))  mesh$materials[[i]]$ambient_texname    = ambient_texture_location  
      if(!is.null(texture_location))          mesh$materials[[i]]$diffuse_texname    = texture_location  
      if(!is.null(emissive_texture_location)) mesh$materials[[i]]$emissive_texname   = emissive_texture_location 
      if(!is.null(specular_texture_location)) mesh$materials[[i]]$specular_texname   = specular_texture_location 
      if(!is.null(normal_texture_location))   mesh$materials[[i]]$normal_texname     = normal_texture_location   
      if(!is.null(diffuse_intensity))         mesh$materials[[i]]$diffuse_intensity  = diffuse_intensity 
      if(!is.null(specular_intensity))        mesh$materials[[i]]$specular_intensity = specular_intensity   
      if(!is.null(emission_intensity))        mesh$materials[[i]]$emission_intensity = emission_intensity  
      if(!is.null(culling))                   mesh$materials[[i]]$culling            = culling   
      if(!is.null(type))                      mesh$materials[[i]]$type               = type   
      
    }
    for(i in seq_len(length(mesh$shapes))) {
      mesh$shapes[[i]]$material_ids = rep(0,nrow(mesh$shapes[[i]]$indices))
    }
  } else {
   stop("No materials detected")
  }
  return(mesh)
}

#'@title Set Material
#'
#'Add shape to a mesh
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
material_list = function(diffuse                   = c(0.8,0.8,0.8),
                         ambient                   = c(0,0,0),
                         specular                  = c(1,1,1),
                         transmittance             = c(1,1,1),
                         emission                  = c(0,0,0),
                         shininess                 = 10.0,
                         ior                       = 1.0,
                         dissolve                  = 1.0,
                         illum                     = 1.0,
                         texture_location          = "",
                         normal_texture_location   = "",
                         specular_texture_location = "",
                         ambient_texture_location  = "",
                         emissive_texture_location = "",
                         diffuse_intensity         = 1, 
                         specular_intensity        = 1,  
                         emission_intensity        = 1,
                         culling                   = "back",
                         type                      = "diffuse") {
  material_props = 
  list(diffuse                   = diffuse                   ,
       ambient                   = ambient                   ,
       specular                  = specular                  ,
       transmittance             = transmittance             ,
       emission                  = emission                  ,
       shininess                 = shininess                 ,
       ior                       = ior                       ,
       dissolve                  = dissolve                  ,
       illum                     = illum                     ,
       texture_location          = texture_location          ,
       normal_texture_location   = normal_texture_location   ,
       specular_texture_location = specular_texture_location ,
       ambient_texture_location  = ambient_texture_location  ,
       emissive_texture_location = emissive_texture_location ,
       diffuse_intensity         = diffuse_intensity         ,
       specular_intensity        = specular_intensity        ,
       emission_intensity        = emission_intensity        ,
       culling                   = culling                   ,
       type                      = type                      )
  return(material_props)
}