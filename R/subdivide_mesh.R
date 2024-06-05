#' @title Subdivide Mesh
#'
#' @description Applies Loop subdivision to the scene (or selected meshes).
#'
#' @param scene The scene to subdivide.
#' @param id Default `NA`, all shapes. The index of which shape to subdivide.
#' @param subdivision_levels Default `1`. 
#' Number of Loop subdivisions to be applied to the mesh.
#' @param verbose Default `FALSE`.
#' @return Scene with shape added.
#' @export
#'
#' @examples
#'if(run_documentation()) {
#' #Subdivide the included R mesh
#' obj_mesh(r_obj(),position=c(-0.5,0,0)) |> 
#'   add_shape(subdivide_mesh(obj_mesh(r_obj(),position=c(0.5,0,0)),
#'                            subdivision_levels = 2)) |> 
#'   rasterize_scene(light_info = directional_light(direction=c(0.2,0.5,1)),fov=13)
#'}
subdivide_mesh = function(scene, id = NA, subdivision_levels = 2, verbose = FALSE) {
  if(is.na(id)) {
    for(i in seq_along(scene$shapes)) {
      new_shape_info = LoopSubdivide(mesh = scene, 
                                shape_i = i-1,
                                nLevels = subdivision_levels, 
                                verbose = verbose)
      scene$shapes[[i]]$indices = new_shape_info$indices
      scene$shapes[[i]]$tex_indices = new_shape_info$indices
      scene$shapes[[i]]$norm_indices = new_shape_info$indices
      scene$shapes[[i]]$material_ids = new_shape_info$face_material_id
      scene$shapes[[i]]$has_vertex_tex = new_shape_info$has_vertex_tex
      scene$shapes[[i]]$has_vertex_normals = new_shape_info$has_vertex_normals
      scene$vertices[[i]] = new_shape_info$vertices
      scene$normals[[i]] = new_shape_info$normals
      scene$texcoords[[i]] = new_shape_info$texcoords
    }
  } else {
    stopifnot(id <= length(scene$shapes))
    new_shape_info = LoopSubdivide(mesh = scene, 
                                   shape_i = id-1,
                                   nLevels = subdivision_levels, 
                                   verbose = verbose)
    scene$shapes[[id]]$indices = new_shape_info$indices
    scene$shapes[[id]]$tex_indices = new_shape_info$indices
    scene$shapes[[id]]$norm_indices = new_shape_info$indices
    scene$shapes[[id]]$material_ids = new_shape_info$face_material_id
    scene$shapes[[id]]$has_vertex_tex = new_shape_info$has_vertex_tex
    scene$shapes[[id]]$has_vertex_normals = new_shape_info$has_vertex_normals
    scene$vertices[[id]] = new_shape_info$vertices
    scene$normals[[id]] = new_shape_info$normals
    scene$texcoords[[id]] = new_shape_info$texcoords
  }
  return(scene)
}