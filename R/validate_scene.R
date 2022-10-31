#' Validate Scene
#'
#' @param scene Make sure that there are no out of bounds issues and all the materials are valid
#'
#' @return Color vector
#' @keywords internal
validate_scene = function(scene) {
  n_shapes = 0L
  
  n_materials = 0L
  n_material_hashes = 0L
  number_materials = 0L
  
  n_shapes = length(scene$shapes)
  
  if(n_shapes != length(scene$vertices)) {
    stop(sprintf("length of scene$shapes[[%d]] (%d) is not equal to length of scene$vertices[[%d]] (%d)",
                 i,n_shapes, i, length(scene$vertices)))
  }
  if(n_shapes != length(scene$texcoords)) {
    stop(sprintf("length of scene[[%d]]$shapes (%d) is not equal to length of scene[[%d]]$texcoords (%d)",
                 i,n_shapes, i, length(scene$texcoords)))
  }
  if(n_shapes != length(scene$normals)) {
    stop(sprintf("length of scene[[%d]]$shapes (%d) is not equal to length of scene[[%d]]$normals (%d)",
                 i,n_shapes, i, length(scene$normals)))
  }
  #Count the total number of shapes, materials, and material hashes
  for(i in seq_len(n_shapes)) {
    if(!(max(scene$shapes[[i]]$indices) < nrow(scene$vertices[[i]]))) {
      stop(sprintf("Max index (%d) in scene$shape[[%d]] greater than number of vertices (%d) in scene$vertices[[%d]]",
                   max(scene$shapes[[i]]$indices),i, nrow(scene$vertices[[i]]), i))
    }
    if(!(max(scene$shapes[[i]]$tex_indices) < nrow(scene$texcoords[[i]]))) {
      stop(sprintf("Max tex index (%d) in scene$shape[[%d]] greater than number of texcoords (%d) in scene$texcoords[[%d]]",
                   max(scene$shapes[[i]]$tex_indices),i, nrow(scene$texcoords[[i]]), i))
    }
    if(!(max(scene$shapes[[i]]$norm_indices) < nrow(scene$normals[[i]]))) {
      stop(sprintf("Max norm index (%d) in scene$shape[[%d]] greater than number of normals (%d) in scene$normals[[%d]]",
                   max(scene$shapes[[i]]$norm_indices),i, nrow(scene$normals[[i]]), i))
    }
  }
  for(i in seq_len(length(scene$materials))) {
    mat_len = length(scene$materials[[i]])
    for(j in seq_len(mat_len)) {
      if(length(scene$materials[[i]][[j]]) != 26) {
        stop(sprintf("Material %d (sub-material %d) does not have the right number of entries", i, j))
      }
    }
  }
  message("Scene passed validation")
}