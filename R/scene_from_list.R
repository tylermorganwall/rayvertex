#'@title Scene From List
#'
#'@description Fast generation of rayvertex scenes from a list of objects (much faster than calling 
#'`add_shape()` on each object individually to build the scene). This returns a `ray_scene` object
#'that cdoes 
#'
#'@param scene_list List containing rayvertex mesh objects.
#' 
#'@return `ray_scene` containing mesh info.
#'@export
#'@examples
#'if(rayvertex:::run_documentation()) {
#'  #Build a scene out of cubes including 87 * 61 = 5307 objects
#'  scene = list()
#'  volcol = rainbow(103)
#'  counter = 1
#'  for(i in 1:nrow(volcano)) {
#'    for(j in 1:ncol(volcano)) {
#'      scene[[counter]] = cube_mesh(position = c(i,(volcano[i,j]-94),j), 
#'                                   material = material_list(diffuse = volcol[volcano[i,j]-92],
#'                                                            ambient = volcol[volcano[i,j]-92],
#'                                                            ambient_intensity = 0.2))
#'      counter = counter + 1
#'    }
#'  }
#'  #Quickly generate the 
#'  new_scene = scene_from_list(scene)
#'  new_scene |> 
#'    rotate_mesh(c(0,10,0), pivot_point = c(44,0,31)) |> 
#'    add_shape(xz_rect_mesh(position=c(44,0,31),scale=500,
#'                           material = material_list(diffuse="lightblue",
#'                                                    ambient = "lightblue",
#'                                                    ambient_intensity = 0.2))) |> 
#'    rasterize_scene(lookfrom=c(500,500,500), lookat = c(44.00, 40.50, 31.00),
#'                    width=800,height=800, fov=0, ortho_dimensions = c(140,140),
#'                    light_info = directional_light(c(-0.6,1,0.6)))
#'}
scene_from_list = function(scene_list) {
  n = length(scene_list)
  n_shapes = 0L
  
  n_materials = 0L
  n_material_hashes = 0L
  
  number_verts = 0L
  number_tex = 0L
  number_norm = 0L
  number_materials = 0L
  
  #Count the total number of vertices, texture coords, and normals for each list entry
  for(i in seq_len(n)) {
    number_verts = number_verts + nrow(scene_list[[i]]$vertices[[1]])
    number_tex = number_tex + nrow(scene_list[[i]]$texcoords[[1]])
    number_norm = number_norm + nrow(scene_list[[i]]$normals[[1]])
  }
  
  #Count the total number of shapes, materials, and material hashes
  for(i in seq_len(n)) {
    n_shapes = n_shapes + length(scene_list[[i]]$shapes)
    n_materials = n_materials + length(scene_list[[i]]$material)
    n_material_hashes = n_material_hashes + length(scene_list[[i]]$material_hashes)
  }
  
  #initialize the `ray_scene` object with the right number of entries
  new_scene = list()
  new_scene$shapes = vector(mode="list",n_shapes)
  new_scene$materials = vector(mode="list",n_materials)
  new_scene$vertices = vector(mode="list",n)
  new_scene$texcoords = vector(mode="list",n)
  new_scene$normals = vector(mode="list",n)
  new_scene$material_hashes = vector(mode="character",n_material_hashes)
  
  #Fill in each shape and corresponding vertex info matrices
  shape_counter = 1L
  mat_counter = 1L
  mat_hash_counter = 1L
  num_materials = 0L
  for(i in seq_len(n)) {
    #Assign the shapes in the nth list entry
    for(j in seq_len(length(scene_list[[i]]$shapes))) {
      new_scene$shapes[[shape_counter]] = scene_list[[i]]$shapes[[j]]
      shape_counter = shape_counter + 1L
    }
    #Assign in the materials in the nth list entry
    for(j in seq_len(length(scene_list[[i]]$materials))) {
      new_scene$materials[[mat_counter]] = scene_list[[i]]$materials[[j]]
      mat_counter = mat_counter + 1L
    }
    
    #Assign the vertex info for the nth list try
    new_scene$vertices[[i]] = scene_list[[i]]$vertices[[1]]
    new_scene$texcoords[[i]] = scene_list[[i]]$texcoords[[1]]
    new_scene$normals[[i]] = scene_list[[i]]$normals[[1]]
    
    #Assign the material hashes for the nth list entry
    for(j in seq_len(length(scene_list[[i]]$materials))) {
      new_scene$material_hashes[[mat_hash_counter]] = scene_list[[i]]$material_hashes[[j]]
      mat_hash_counter = mat_hash_counter + 1L
    }
  }

  new_scene$vertices = list(do.call(rbind,new_scene$vertices))
  new_scene$texcoords = list(do.call(rbind,new_scene$texcoords))
  new_scene$normals = list(do.call(rbind,new_scene$normals))
  new_scene$material_hashes = unlist(new_scene$material_hashes)
  
  
  class(new_scene) = c("ray_mesh", "list")
  return(new_scene)
}