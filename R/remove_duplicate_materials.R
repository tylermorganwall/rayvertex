#'@title Remove Duplicates
#'
#'@param scene The scene
#'@return Scene with shape added.
#'
#'@keywords internal
remove_duplicate_materials = function(scene) {
  #Generate unique set of materials in scene
  scene_material_hashes = attr(scene, "material_hashes")
  unique_materials = unique(scene_material_hashes)
  #Allocate new_id vector
  new_ids = rep(0L,length(scene_material_hashes))
  
  #Generate vector for all old non-unique IDs (zero indexed)
  old_ids = seq_along(scene$materials) - 1L
  new_mat = list()
  #Go through each hash and determine which entry it is in the unique_material vector
  for(i in seq_along(scene$materials)) {
    new_ids[i] = min(which(scene_material_hashes[i] == unique_materials)) - 1L
  }
  # for(i in seq_len(length(scene$shapes))) {
  #   scene$shapes[[i]]$material_ids[scene$shapes[[i]]$material_ids == -1L] = 0L
  #   scene$shapes[[i]]$material_ids = new_ids[scene$shapes[[i]]$material_ids + 1L]
  # }
  scene$shapes[[1]]$material_ids[scene$shapes[[1]]$material_ids == -1L] = 0L
  scene$shapes[[1]]$material_ids = new_ids[scene$shapes[[1]]$material_ids + 1L]
  unique_ids = unique(new_ids)
  new_mat = list()
  for(i in seq_len(length(unique_ids))) {
    new_mat[[i]] = scene$materials[[min(which(new_ids == (i-1L)))]]
  }
  scene$materials = new_mat
  attr(scene, "material_hashes") = unique_materials
  class(scene) = c("ray_mesh", "list")
  
  return(scene)
}