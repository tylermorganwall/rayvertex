#'@title Scene From List
#'
#'@description Fast generation of rayvertex scenes from a list of objects (much faster than calling 
#'[add_shape()] on each object individually to build the scene). This returns a `ray_scene` object
#'that cdoes 
#'
#'@param scene_list List containing rayvertex mesh objects.
#' 
#'@return `ray_scene` containing mesh info.
#'@export
#'@examples
#'if(run_documentation()) {
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
  scene_list = Filter(Negate(is.null), scene_list)
  n = length(scene_list)
  n_shapes = 0L
  
  n_materials = 0L
  n_material_hashes = 0L
  number_materials = 0L
  
  #Count the total number of shapes, materials, and material hashes
  for(i in seq_len(n)) {
    n_shapes = n_shapes + length(scene_list[[i]]$shapes)
    n_materials = n_materials + length(scene_list[[i]]$materials)
    n_material_hashes = n_material_hashes + length(attr(scene_list[[i]], "material_hashes"))
  }

  #initialize the `ray_scene` object with the right number of entries
  new_scene = list()
  new_scene$shapes = vector(mode="list",n)
  new_scene$vertices = vector(mode="list",n)
  new_scene$texcoords = vector(mode="list",n)
  new_scene$normals = vector(mode="list",n)
  new_scene$materials = vector(mode="list",n)
  attr(new_scene, "material_hashes") = vector(mode="character")
  #Fill in each shape and corresponding vertex info matrices
  for(i in seq_len(n)) {
    #Assign the shapes in the nth list entry
    temp_scene = merge_scene(scene_list[[i]], TRUE)
    new_scene$shapes[[i]] = ray_shape(temp_scene$shapes[[1]])
    
    #Assign the vertex info for the nth list try
    new_scene$vertices[[i]] = ray_vertex_data((temp_scene$vertices))
    new_scene$texcoords[[i]] = ray_vertex_data((temp_scene$texcoords))
    new_scene$normals[[i]] = ray_vertex_data((temp_scene$normals))
    for(j in seq_along(temp_scene$materials)) {
      new_scene$materials[[i]][[j]] = (temp_scene$materials[[j]])
    }
    attr(new_scene, "material_hashes") = c(attr(new_scene, "material_hashes"), 
                                           attr(scene_list[[i]], "material_hashes"))
  }
  new_scene$shapes = do.call(vctrs::vec_c, new_scene$shapes)
  new_scene$vertices = do.call(vctrs::vec_c, new_scene$vertices)
  new_scene$texcoords = do.call(vctrs::vec_c, new_scene$texcoords)
  new_scene$normals = do.call(vctrs::vec_c, new_scene$normals)
  # new_scene$materials = do.call(vctrs::vec_c, new_scene$materials)
  
  class(new_scene) = c("ray_mesh", "list")
  return(new_scene)
}
