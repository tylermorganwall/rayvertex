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
  mesh$vertices  = mesh$vertices + matrix(position,nrow = nrow(mesh$vertices), ncol = 3,byrow=TRUE)
  return(mesh)
}

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
scale_mesh = function(mesh, scale = 1) {
  if(length(scale) == 1) {
    scale = rep(scale,3)
  }
  mesh$vertices[,1]  = mesh$vertices[,1]*scale[1]
  mesh$vertices[,2]  = mesh$vertices[,2]*scale[2]
  mesh$vertices[,3]  = mesh$vertices[,3]*scale[3]
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
set_material = function(mesh, exponent=32, 
                        ambient          = c(0,0,0),
                        diffuse          = c(0.5,0.5,0.5),
                        specular         = c(1,1,1),
                        transmittance    = c(1,1,1),
                        emission         = c(0,0,0),
                        shininess        = 10.0,
                        ior              = 1.0,
                        dissolve         = 1.0,
                        illum            = 1.0,
                        texture_location = "",
                        normal_texture_location = "",
                        specular_texture_location = "",
                        ambient_texture_location  = "",
                        emissive_texture_location = "",
                        diffuse_intensity = 1, specular_intensity = 0.6, emission_intensity = 1) {
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    for(i in seq_len(length(mesh$materials))) {
      mesh$materials[[i]] = list()
      mesh$materials[[i]]$ambient          = convert_color(ambient)
      mesh$materials[[i]]$diffuse          = convert_color(diffuse)
      mesh$materials[[i]]$specular         = convert_color(specular)
      mesh$materials[[i]]$transmittance    = convert_color(transmittance)
      mesh$materials[[i]]$emission         = convert_color(emission)
      mesh$materials[[i]]$shininess        = shininess
      mesh$materials[[i]]$ior              = ior              
      mesh$materials[[i]]$dissolve         = dissolve         
      mesh$materials[[i]]$illum            = illum            
      mesh$materials[[i]]$ambient_texname  = ambient_texture_location  
      mesh$materials[[i]]$diffuse_texname  = texture_location  
      mesh$materials[[i]]$emissive_texname = emissive_texture_location 
      mesh$materials[[i]]$specular_texname = specular_texture_location 
      mesh$materials[[i]]$normal_texname   = normal_texture_location   
    }
    for(i in seq_len(length(mesh$shapes))) {
      mesh$shapes[[i]]$material_ids = rep(0,nrow(mesh$shapes[[i]]$indices))
    }
  } else {
    mesh$shapes[[1]]$material_ids = rep(0,nrow(mesh$shapes[[1]]$indices))
    
    mesh$materials[[1]] = list()
    mesh$materials[[1]]$ambient          = convert_color(ambient)
    mesh$materials[[1]]$diffuse          = convert_color(diffuse)
    mesh$materials[[1]]$specular         = convert_color(specular)
    mesh$materials[[1]]$transmittance    = convert_color(transmittance)
    mesh$materials[[1]]$emission         = convert_color(emission)
    mesh$materials[[1]]$shininess        = shininess
    mesh$materials[[1]]$ior              = ior              
    mesh$materials[[1]]$dissolve         = dissolve         
    mesh$materials[[1]]$illum            = illum            
    mesh$materials[[1]]$ambient_texname  = ambient_texture_location  
    mesh$materials[[1]]$diffuse_texname  = texture_location  
    mesh$materials[[1]]$emissive_texname = emissive_texture_location 
    mesh$materials[[1]]$specular_texname = specular_texture_location 
    mesh$materials[[1]]$normal_texname   = normal_texture_location   
  }
  return(mesh)
}