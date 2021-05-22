#'@title Add Shape
#'
#'Add shape to the scene
#'
#'@param scene The scene to add the shape.
#'@param shape The mesh to add to the scene.

#'@return Scene with shape added.
#'@export
#'
#'@examples
#'#Generate several spheres in the cornell box
#'scene = generate_cornell_mesh()
#'set.seed(1)
#'
#'for(i in 1:30) {
#'  col = hsv(runif(1))
#'  scene = add_shape(scene, sphere_mesh(position=runif(3)*400+155/2,
#'                                       material=material_list(diffuse=col, type="phong",
#'                                                              ambient=col,ambient_intensity=0.2), 
#'                                       radius=30))
#'}
#'rasterize_scene(scene, light_info=directional_light(direction=c(0.1,0.6,-1)))
add_shape = function(scene, shape) {
  if(length(scene) == 0) {
    return(shape)
  }
  max_vertices = nrow(scene$vertices)
  max_norms = nrow(scene$normals)
  max_tex = nrow(scene$texcoords)
  max_material = length(scene$materials)
  
  
  for(i in seq_len(length(shape$shapes))) {
    shape$shapes[[i]]$indices      = shape$shapes[[i]]$indices      + max_vertices
    shape$shapes[[i]]$tex_indices  = shape$shapes[[i]]$tex_indices  + max_tex
    shape$shapes[[i]]$norm_indices = shape$shapes[[i]]$norm_indices + max_norms
    shape$shapes[[i]]$material_ids = ifelse(shape$shapes[[i]]$material_ids != -1, 
                                            shape$shapes[[i]]$material_ids + max_material,
                                            -1)
  }
  scene$shapes = c(scene$shapes,shape$shapes)

  scene$vertices  = rbind(scene$vertices,  shape$vertices)
  scene$normals   = rbind(scene$normals,   shape$normals)
  scene$texcoords = rbind(scene$texcoords, shape$texcoords)
  
  scene$materials = c(scene$materials,shape$materials)
  scene$material_hashes = c(scene$material_hashes,shape$material_hashes)
  
  scene = remove_duplicate_materials(scene)
  
  if(!is.null(attr(shape,"cornell")) || !is.null(attr(scene,"cornell"))) {
    attr(scene,"cornell") = TRUE
    if(!is.null(attr(shape,"cornell"))) {
      attr(shape,"cornell_light") = attr(shape,"cornell_light")
    } else {
      attr(scene,"cornell_light") = attr(scene,"cornell_light")
    }
  }
  return(scene)
}

#'@title Add Shape
#'
#'Add shape to the scene
#'
#'@param scene The scene to add the shape.

#'@return Scene with shape added.
#'
#'@examples
#'#Generate several spheres in the cornell box
#'scene = generate_cornell_mesh()
#'set.seed(1)
#'
#'for(i in 1:30) {
#'  col = hsv(runif(1))
#'  scene = add_shape(scene, sphere_mesh(position=runif(3)*400+155/2,
#'                                       material=material_list(diffuse=col, type="phong",
#'                                                              ambient=col,ambient_intensity=0.2), 
#'                                       radius=30))
#'}
#'rasterize_scene(scene, light_info=directional_light(direction=c(0.1,0.6,-1)))
remove_duplicate_materials = function(scene) {
  if(length(scene$materials) == 1 || length(scene$materials) == 0) {
    return(scene)
  }
  
  scene_material_hashes = scene$material_hashes
  unique_materials = unique(scene_material_hashes)
  new_ids = rep(0,length(scene_material_hashes))
  old_ids = seq_len(length(scene_material_hashes)) - 1
  
  for(i in seq_len(length(scene_material_hashes))) {
    new_ids[i] = min(which(scene_material_hashes[i] == unique_materials)) - 1
  }
  
  for(i in seq_len(length(scene$shapes))) {
    tmp_ids = scene$shapes[[i]]$material_ids
    scene$shapes[[i]]$material_ids[tmp_ids %in% old_ids] = new_ids[match(tmp_ids, old_ids, nomatch = 0)]
  }
  unique_ids = unique(new_ids)
  new_mat = list()
  for(i in seq_len(length(unique_ids))) {
    new_mat[[i]] = scene$materials[[unique_ids[i]+1]]
  }
  scene$materials = new_mat
  scene$material_hashes = unique_materials
  
  return(scene)
}


#'@title Merge shapes
#'
#'Add shape to a mesh
#'
#'@param scene  
#'@keywords internal
#'@return Merged scene
merge_shapes = function(scene) {
  indices = list()
  tex_indices = list()
  norm_indices = list()
  material_ids = list()
  has_vertex_tex = list()
  has_vertex_normals = list()
  
  for(i in seq_len(length(scene$shapes))) {
    indices[[i]]      = scene$shapes[[i]]$indices     
    tex_indices[[i]]  = scene$shapes[[i]]$tex_indices 
    norm_indices[[i]] = scene$shapes[[i]]$norm_indices
    material_ids[[i]] = scene$shapes[[i]]$material_ids
    has_vertex_tex[[i]] = scene$shapes[[i]]$has_vertex_tex
    has_vertex_normals[[i]] = scene$shapes[[i]]$has_vertex_normals
  }
  
  scene$shapes = list()
  scene$shapes[[1]] = list()
  
  scene$shapes[[1]]$indices = do.call(rbind,indices)
  scene$shapes[[1]]$tex_indices = do.call(rbind,tex_indices)
  scene$shapes[[1]]$norm_indices = do.call(rbind,norm_indices)
  scene$shapes[[1]]$material_ids = unlist(material_ids)
  scene$shapes[[1]]$has_vertex_tex = unlist(has_vertex_tex)
  scene$shapes[[1]]$has_vertex_normals = unlist(has_vertex_normals)
  
  return(scene)
}

#'@title Translate Mesh
#'
#'@param mesh The mesh.
#'@param position Default `c(0,0,0)`. The translation vector. 
#'
#'@return Translated mesh
#'@export
#'@examples
#'#Translate a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'generate_cornell_mesh() %>% 
#'  add_shape(translate_mesh(robj,c(400,0,155))) %>% 
#'  add_shape(translate_mesh(robj,c(555/2,100,555/2))) %>% 
#'  add_shape(translate_mesh(robj,c(155,200,400))) %>% 
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
translate_mesh = function(mesh, position = c(0,0,0)) {
  mesh$vertices[,1]  = mesh$vertices[,1] + position[1]
  mesh$vertices[,2]  = mesh$vertices[,2] + position[2]
  mesh$vertices[,3]  = mesh$vertices[,3] + position[3]
  return(mesh)
}

#'@title Scale Mesh
#'
#'@param mesh The mesh.
#'@param scale Default `c(1,1,1)`. The scale amount, per axis. 
#'@param center Default `c(0,0,0)`. The center of the scale.
#'
#'@return Scaled mesh
#'@export
#'@examples
#'#Scale a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'
#'generate_cornell_mesh() %>% 
#' add_shape(scale_mesh(translate_mesh(robj,c(400,0,155)),0.5, center=c(400,0,155))) %>% 
#' add_shape(scale_mesh(translate_mesh(robj,c(555/2,100,555/2)),1.5, center=c(555/2,100,555/2))) %>% 
#' add_shape(scale_mesh(translate_mesh(robj,c(155,200,400)),c(0.5,2,0.5), center=c(155,200,400))) %>% 
#' rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
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

#'@title Generate Rotation Matrix
#'
#'@param angle The angle
#'@param order_rotation Default `c(1,2,3)`. 
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

#'@title Rotate Mesh
#'
#'@param mesh The mesh.
#'@param angle Default `c(0,0,0)`. The rotation amount for the x/y/z axes, in degrees.
#'@param pivot_point Default `c(0,0,0)`. The pivot point of the rotation.
#'@param order_rotation Default `c(1,2,3)`. The order in which to perform the rotations.
#'
#'@return Rotated Mesh
#'@export
#'@examples
#'#Rotate a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'
#'generate_cornell_mesh() %>% 
#' add_shape(rotate_mesh(translate_mesh(robj,c(400,0,155)),c(0,30,0), 
#'                       pivot_point=c(400,0,155))) %>% 
#' add_shape(rotate_mesh(translate_mesh(robj,c(555/2,100,555/2)),c(-30,60,30), 
#'                       pivot_point=c(555/2,100,555/2))) %>% 
#' add_shape(rotate_mesh(translate_mesh(robj,c(155,200,400)),c(-30,60,30), 
#'                       pivot_point=c(155,200,400), order_rotation=c(3,2,1))) %>% 
#' rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
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
#'@param mesh The target mesh. 
#'@param material Default `NULL`. You can pass the output of the `material_list()` function
#'to specify the material, or use the following individual settings.
#'@param diffuse                   Default `c(0.5,0.5,0.5)`. The diffuse color.
#'@param ambient                   Default `c(0,0,0)`. The ambient color.
#'@param specular                  Default `c(1,1,1)`. The specular color.
#'@param transmittance             Default `c(1,1,1)`. The transmittance
#'@param emission                  Default `c(0,0,0)`. The emissive color.
#'@param shininess                 Default `10.0`. The shininess exponent.
#'@param ior                       Default `1.0`. The index of refraction.
#'@param dissolve                  Default `1.0`. The transparency.
#'@param illum                     Default `1.0`. The illumination.
#'@param texture_location          Default `""`. The diffuse texture location.
#'@param normal_texture_location   Default `""`. The normal texture location.
#'@param specular_texture_location Default `""`. The specular texture location.
#'@param ambient_texture_location  Default `""`. The ambient texture location.
#'@param emissive_texture_location Default `""`. The emissive texture location.
#'@param diffuse_intensity         Default `1`. The diffuse intensity.
#'@param specular_intensity        Default `1`. The specular intensity.
#'@param emission_intensity        Default `1`. The emission intensity.
#'@param ambient_intensity         Default `1`. The ambient intensity.
#'@param culling                   Default `"back"`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `"diffuse"`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `TRUE`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `5`. Number of color breaks in the toon shader. 
#'
#'@return Shape with new material
#'@export
#'@examples
#'#Set the material of an object
#'generate_cornell_mesh() %>% 
#'  add_shape(set_material(sphere_mesh(position=c(400,555/2,555/2),radius=40), 
#'                         diffuse="purple", type="phong")) %>% 
#'  add_shape(set_material(sphere_mesh(position=c(555/2,220,555/2),radius=40),
#'                         dissolve=0.2,culling="none",diffuse="red")) %>% 
#'  add_shape(set_material(sphere_mesh(position=c(155,300,555/2),radius=60), 
#'                         material = material_list(diffuse="gold", type="phong", 
#'                                                  ambient="gold", ambient_intensity=0.4))) %>% 
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
set_material = function(mesh, material = NULL, id = NULL,
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
                        ambient_intensity         = 1,
                        culling                   = "back",
                        type                      = "diffuse",
                        translucent               = TRUE,
                        toon_levels               = 5,
                        toon_outline_width        = 1.01,
                        toon_outline_color        = "black") {
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
    ambient_intensity         = material$ambient_intensity        
    culling                   = material$culling                   
    type                      = material$type     
    translucent               = material$translucent
    toon_levels               = material$toon_levels      
    toon_outline_width        = material$toon_outline_width       
    toon_outline_color        = material$toon_outline_color        
  }
  
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    if(is.null(id)) {
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
        mesh$materials[[i]]$ambient_intensity  = ambient_intensity  
        mesh$materials[[i]]$culling            = culling   
        mesh$materials[[i]]$type               = type   
        mesh$materials[[i]]$translucent        = translucent
        mesh$materials[[i]]$toon_levels        = toon_levels   
        mesh$materials[[i]]$toon_outline_width = toon_outline_width   
        mesh$materials[[i]]$toon_outline_color = convert_color(toon_outline_color)
        
      }
      mesh$material_hashes[i] = digest::digest(mesh$materials[[i]])
      for(i in seq_len(length(mesh$shapes))) {
        mesh$shapes[[i]]$material_ids = rep(0,nrow(mesh$shapes[[i]]$indices))
      }
    } else {
      mesh$materials[[id]] = list()
      mesh$materials[[id]]$ambient            = convert_color(ambient)
      mesh$materials[[id]]$diffuse            = convert_color(diffuse)
      mesh$materials[[id]]$specular           = convert_color(specular) 
      mesh$materials[[id]]$transmittance      = convert_color(transmittance)
      mesh$materials[[id]]$emission           = convert_color(emission) 
      mesh$materials[[id]]$shininess          = shininess
      mesh$materials[[id]]$ior                = ior              
      mesh$materials[[id]]$dissolve           = dissolve         
      mesh$materials[[id]]$illum              = illum            
      mesh$materials[[id]]$ambient_texname    = ambient_texture_location  
      mesh$materials[[id]]$diffuse_texname    = texture_location  
      mesh$materials[[id]]$emissive_texname   = emissive_texture_location 
      mesh$materials[[id]]$specular_texname   = specular_texture_location 
      mesh$materials[[id]]$normal_texname     = normal_texture_location   
      mesh$materials[[id]]$diffuse_intensity  = diffuse_intensity 
      mesh$materials[[id]]$specular_intensity = specular_intensity   
      mesh$materials[[id]]$emission_intensity = emission_intensity  
      mesh$materials[[id]]$ambient_intensity  = ambient_intensity  
      mesh$materials[[id]]$culling            = culling   
      mesh$materials[[id]]$type               = type   
      mesh$materials[[id]]$translucent        = translucent
      mesh$materials[[id]]$toon_levels        = toon_levels   
      mesh$materials[[id]]$toon_outline_width = toon_outline_width   
      mesh$materials[[id]]$toon_outline_color = convert_color(toon_outline_color)
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
    mesh$materials[[1]]$ambient_intensity  = ambient_intensity  
    mesh$materials[[1]]$culling            = culling    
    mesh$materials[[1]]$type               = type    
    mesh$materials[[1]]$translucent        = translucent   
    mesh$materials[[1]]$toon_levels        = toon_levels   
    mesh$materials[[1]]$toon_outline_width = toon_outline_width   
    mesh$materials[[1]]$toon_outline_color = convert_color(toon_outline_color)
    mesh$material_hashes[1] = digest::digest(mesh$materials[[1]])
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

#'@title Change Material
#'
#'Change individual material properties, leaving others alone.
#'
#'@param mesh Mesh to change.
#'@param id Default `NULL`. Either a number specifying the material to change, or a character vector 
#'matching the material name.
#'@param diffuse                   Default `NULL`. The diffuse color.
#'@param ambient                   Default `NULL`. The ambient color.
#'@param specular                  Default `NULL`. The specular color.
#'@param transmittance             Default `NULL`. The transmittance
#'@param emission                  Default `NULL`. The emissive color.
#'@param shininess                 Default `NULL`. The shininess exponent.
#'@param ior                       Default `NULL`. The index of refraction.
#'@param dissolve                  Default `NULL`. The transparency.
#'@param illum                     Default `NULL`. The illumination.
#'@param texture_location          Default `NULL`. The diffuse texture location.
#'@param normal_texture_location   Default `NULL`. The normal texture location.
#'@param specular_texture_location Default `NULL`. The specular texture location.
#'@param ambient_texture_location  Default `NULL`. The ambient texture location.
#'@param emissive_texture_location Default `NULL`. The emissive texture location.
#'@param diffuse_intensity         Default `NULL`. The diffuse intensity.
#'@param specular_intensity        Default `NULL`. The specular intensity.
#'@param emission_intensity        Default `NULL`. The emission intensity.
#'@param ambient_intensity         Default `NULL`. The ambient intensity.
#'@param culling                   Default `NULL`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `NULL`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `TRUE`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `5`. Number of color breaks in the toon shader.
#'
#'@return Shape with new material settings
#'@export
#'@examples
#'p_sphere = sphere_mesh(position=c(555/2,555/2,555/2), 
#'                       radius=40,material=material_list(diffuse="purple"))
#'                       
#'generate_cornell_mesh() %>% 
#'  add_shape(p_sphere) %>% 
#'  add_shape(change_material(translate_mesh(p_sphere,c(200,0,0)),diffuse="red")) %>% 
#'  add_shape(change_material(translate_mesh(p_sphere,c(100,0,0)),dissolve=0.5)) %>% 
#'  add_shape(change_material(translate_mesh(p_sphere,c(-100,0,0)),type="phong")) %>% 
#'  add_shape(change_material(translate_mesh(p_sphere,c(-200,0,0)),type="phong",shininess=30)) %>% 
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
change_material = function(mesh, id = NULL, 
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
                           ambient_intensity         = NULL,
                           culling                   = NULL,
                           type                      = NULL,
                           translucent               = NULL,
                           toon_levels               = NULL,
                           toon_outline_width        = NULL,
                           toon_outline_color        = NULL) {
  if(!is.null(culling)) {
    culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  }
  
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    if(is.null(id)) {
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
        if(!is.null(ambient_intensity))         mesh$materials[[i]]$ambient_intensity  = ambient_intensity  
        if(!is.null(culling))                   mesh$materials[[i]]$culling            = culling   
        if(!is.null(type))                      mesh$materials[[i]]$type               = type   
        if(!is.null(translucent))               mesh$materials[[i]]$translucent        = translucent   
        if(!is.null(toon_levels))               mesh$materials[[i]]$toon_levels        = toon_levels   
        if(!is.null(toon_outline_width))        mesh$materials[[i]]$toon_outline_width = toon_outline_width   
        if(!is.null(toon_outline_color))        mesh$materials[[i]]$toon_outline_color = convert_color(toon_outline_color)
      }
    } else {
      if(is.numeric(id)) {
        if(!is.null(ambient))                   mesh$materials[[id]]$ambient            = convert_color(ambient)
        if(!is.null(diffuse))                   mesh$materials[[id]]$diffuse            = convert_color(diffuse)
        if(!is.null(specular))                  mesh$materials[[id]]$specular           = convert_color(specular)
        if(!is.null(transmittance))             mesh$materials[[id]]$transmittance      = convert_color(transmittance)
        if(!is.null(emission))                  mesh$materials[[id]]$emission           = convert_color(emission)
        if(!is.null(shininess))                 mesh$materials[[id]]$shininess          = shininess
        if(!is.null(ior))                       mesh$materials[[id]]$ior                = ior              
        if(!is.null(dissolve))                  mesh$materials[[id]]$dissolve           = dissolve         
        if(!is.null(illum))                     mesh$materials[[id]]$illum              = illum            
        if(!is.null(ambient_texture_location))  mesh$materials[[id]]$ambient_texname    = ambient_texture_location  
        if(!is.null(texture_location))          mesh$materials[[id]]$diffuse_texname    = texture_location  
        if(!is.null(emissive_texture_location)) mesh$materials[[id]]$emissive_texname   = emissive_texture_location 
        if(!is.null(specular_texture_location)) mesh$materials[[id]]$specular_texname   = specular_texture_location 
        if(!is.null(normal_texture_location))   mesh$materials[[id]]$normal_texname     = normal_texture_location   
        if(!is.null(diffuse_intensity))         mesh$materials[[id]]$diffuse_intensity  = diffuse_intensity 
        if(!is.null(specular_intensity))        mesh$materials[[id]]$specular_intensity = specular_intensity   
        if(!is.null(emission_intensity))        mesh$materials[[id]]$emission_intensity = emission_intensity  
        if(!is.null(ambient_intensity))         mesh$materials[[id]]$ambient_intensity  = ambient_intensity  
        if(!is.null(culling))                   mesh$materials[[id]]$culling            = culling   
        if(!is.null(type))                      mesh$materials[[id]]$type               = type   
        if(!is.null(translucent))               mesh$materials[[id]]$translucent        = translucent  
        if(!is.null(toon_levels))               mesh$materials[[id]]$toon_levels        = toon_levels   
        if(!is.null(toon_outline_width))        mesh$materials[[id]]$toon_outline_width = toon_outline_width   
        if(!is.null(toon_outline_color))        mesh$materials[[id]]$toon_outline_color = convert_color(toon_outline_color) 
      }
      if(is.character(id)) {
        for(i in seq_len(length(mesh$materials))) {
          if(names(mesh$materials)[i] == id) {
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
            if(!is.null(ambient_intensity))         mesh$materials[[i]]$ambient_intensity  = ambient_intensity  
            if(!is.null(culling))                   mesh$materials[[i]]$culling            = culling   
            if(!is.null(type))                      mesh$materials[[i]]$type               = type   
            if(!is.null(translucent))               mesh$materials[[i]]$translucent        = translucent   
            if(!is.null(toon_levels))               mesh$materials[[i]]$toon_levels        = toon_levels   
            if(!is.null(toon_outline_width))        mesh$materials[[i]]$toon_outline_width = toon_outline_width   
            if(!is.null(toon_outline_color))        mesh$materials[[i]]$toon_outline_color = convert_color(toon_outline_color)
          }
        }
      }
    }
    for(i in seq_len(length(mesh$materials))) {
      mesh$material_hashes[i] = digest::digest(mesh$materials[[i]])
    }
  } else {
   stop("No materials detected")
  }
  return(mesh)
}

#'@title Material List
#'
#'Generate a material properties list.
#'
#'@param diffuse                   Default `c(0.5,0.5,0.5)`. The diffuse color.
#'@param ambient                   Default `c(0,0,0)`. The ambient color.
#'@param specular                  Default `c(1,1,1)`. The specular color.
#'@param transmittance             Default `c(1,1,1)`. The transmittance
#'@param emission                  Default `c(0,0,0)`. The emissive color.
#'@param shininess                 Default `10.0`. The shininess exponent.
#'@param ior                       Default `1.0`. The index of refraction.
#'@param dissolve                  Default `1.0`. The transparency.
#'@param illum                     Default `1.0`. The illumination.
#'@param texture_location          Default `""`. The diffuse texture location.
#'@param normal_texture_location   Default `""`. The normal texture location.
#'@param specular_texture_location Default `""`. The specular texture location.
#'@param ambient_texture_location  Default `""`. The ambient texture location.
#'@param emissive_texture_location Default `""`. The emissive texture location.
#'@param diffuse_intensity         Default `1`. The diffuse intensity.
#'@param specular_intensity        Default `1`. The specular intensity.
#'@param emission_intensity        Default `1`. The emission intensity.
#'@param ambient_intensity         Default `1`. The ambient intensity.
#'@param culling                   Default `"back"`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `"diffuse"`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `FALSE`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `5`. Number of color breaks in the toon shader.
#'@param toon_outline_width        Default `1.01`. Expansion term for model to specify toon outline width.
#'@param toon_outline_color        Default `black`. Toon outline color.
#'
#'@return List of material properties.
#'@export
#'@examples
#'mat_prop = material_list(diffuse="purple", type="phong", shininess=20,
#'                         ambient="purple", ambient_intensity=0.3,
#'                         specular = "red", specular_intensity=2)
#'                         
#'p_sphere = sphere_mesh(position=c(555/2,555/2,555/2), 
#'                       radius=40,material=mat_prop)
#'
#'rasterize_scene(p_sphere, light_info=directional_light(direction=c(0.1,0.6,-1)))
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
                         ambient_intensity         = 1,
                         culling                   = "back",
                         type                      = "diffuse",
                         translucent               = TRUE,
                         toon_levels               = 5,
                         toon_outline_width        = 1.01,
                         toon_outline_color        = "black") {
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
       ambient_intensity         = ambient_intensity         ,
       culling                   = culling                   ,
       type                      = type                      ,
       translucent               = translucent               ,
       toon_levels               = toon_levels               ,
       toon_outline_width        = toon_outline_width        ,
       toon_outline_color        = toon_outline_color        )
  return(material_props)
}

#' Add Outline
#'
#'@param angle The angle
#'@param order_rotation Default `c(1,2,3)`. 
#'@return Matrix
#'@keywords internal
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
#' 
generate_toon_outline = function(single_obj, material, scale = 1) {
  if((material$type == "toon" || material$type == "toon_phong") && material$toon_outline_width != 0.0) {
    bbox = apply(single_obj$vertices,2,range)
    bbox_size = bbox[2,] - bbox[1,]
    scaleval = (bbox_size + material$toon_outline_width)/bbox_size
    single_obj = single_obj %>% 
      scale_mesh(scale = scaleval) %>% 
      set_material(diffuse=material$toon_outline_color , culling = "front", type="color")
  }
  return(single_obj)
}