#'@title Merge scene
#'
#'@description Merge the shapes to one
#'
#'@keywords internal
#'@return Merged scene
merge_scene = function(old_scene, flatten_materials = TRUE) {
  if(length(old_scene$shapes) == 1) {
    old_scene$vertices = old_scene$vertices[[1]]
    old_scene$texcoords = old_scene$texcoords[[1]]
    old_scene$normals = old_scene$normals[[1]]
    old_scene$materials = old_scene$materials[[1]]
    
    return(old_scene)
  }
  indices = list()
  tex_indices = list()
  norm_indices = list()
  material_ids = list()
  has_vertex_tex = list()
  has_vertex_normals = list()
  materials = list()
  
  max_vertices = 0L
  max_texcoords = 0L
  max_normals = 0L
  max_materials = 0L
  for(i in seq_len(length(old_scene$shapes))) {
    indices[[i]]      = old_scene$shapes[[i]]$indices      + max_vertices
    tex_indices[[i]]  = old_scene$shapes[[i]]$tex_indices  + max_texcoords
    norm_indices[[i]] = old_scene$shapes[[i]]$norm_indices + max_normals
    material_ids[[i]] = ifelse(old_scene$shapes[[i]]$material_ids != -1L,
                               old_scene$shapes[[i]]$material_ids + max_materials,
                               -1L)
    has_vertex_tex[[i]] = old_scene$shapes[[i]]$has_vertex_tex
    has_vertex_normals[[i]] = old_scene$shapes[[i]]$has_vertex_normals
    
    max_vertices = max_vertices + nrow(old_scene$vertices[[i]]) 
    max_texcoords = max_texcoords + nrow(old_scene$texcoords[[i]]) 
    max_normals = max_normals + nrow(old_scene$normals[[i]])
    max_materials = max_materials + length(old_scene$materials[[i]])
  }
  
  scene = list()
  scene$shapes = list()
  scene$shapes[[1]] = list()
  
  scene$shapes[[1]]$indices = do.call(rbind,indices)
  scene$shapes[[1]]$tex_indices = do.call(rbind,tex_indices)
  scene$shapes[[1]]$norm_indices = do.call(rbind,norm_indices)
  scene$shapes[[1]]$material_ids = unlist(material_ids)
  scene$shapes[[1]]$has_vertex_tex = unlist(has_vertex_tex)
  scene$shapes[[1]]$has_vertex_normals = unlist(has_vertex_normals)
  scene$vertices = unlist(do.call(rbind, old_scene$vertices), recursive = FALSE)
  scene$texcoords = unlist(do.call(rbind, old_scene$texcoords), recursive = FALSE)
  scene$normals = unlist(do.call(rbind, old_scene$normals), recursive = FALSE)
  
  #Flatten materials
  num_materials = 0
  for(i in seq_len(length(old_scene$materials))) {
    num_materials = num_materials + length(old_scene$materials[[i]]) 
  }
  if(flatten_materials) {
    scene$materials = vector(mode="list", length = num_materials)
  } else {
    scene$materials = list(list(vector(mode="list", length = num_materials)))
  }
  
  counter = 1
  for(i in seq_len(length(old_scene$materials))) {
    n_mats = length(old_scene$materials[[i]])  
    for(j in seq_len(n_mats)) {
      if(flatten_materials) {
        scene$materials[[counter]] = old_scene$materials[[i]][[j]] 
        counter = counter + 1
      } else {
        scene$materials[[1]][[j]] = old_scene$materials[[i]][[j]] 
      }
    }
  }
  attr(scene, "material_hashes") = attr(old_scene, "material_hashes")
  
  class(scene) = c("ray_mesh", "list")
  
  return(scene)
}