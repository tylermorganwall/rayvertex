#'@title Remove Duplicates
#'
#' @param scene The scene.
#'@return Scene with shape added.
#'
#'@keywords internal
remove_duplicate_materials = function(scene) {
  #Generate unique set of materials in scene
  scene_material_hashes = attr(scene, "material_hashes")
  unique_materials = unique(scene_material_hashes)
  # Stable first-occurrence mapping, with the existing default-material sentinel.
  new_ids = match(scene_material_hashes, unique_materials) - 1L
  representatives = match(unique_materials, scene_material_hashes)
  ids = scene$shapes[[1]]$material_ids
  ids[ids == -1L] = 0L
  scene$shapes[[1]]$material_ids = new_ids[ids + 1L]
  scene$materials = lapply(representatives, function(i) scene$materials[[i]])
  attr(scene, "material_hashes") = unique_materials
  class(scene) = c("ray_mesh", "list")

  return(scene)
}
