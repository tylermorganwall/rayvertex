#' Swap Y/Z Axis
#'
#' @param mesh The mesh to swap orientations.
#'
#' @return Mesh with Y and Z axis exchanged
#' @export 
#' @examples
#' # Flip a mesh 
#' if(run_documentation()) {
#' sphere_mesh(position=c(-1,0,0)) |> 
#'   add_shape(swap_yz(sphere_mesh(position=c(0,1,0)))) |> 
#'   rasterize_scene()
#' }
swap_yz = function(mesh) {
  for(i in seq_along(mesh$vertices)) {
    mesh$vertices[[i]] = mesh$vertices[[i]][,c(1,3,2)]
  }
  for(i in seq_along(mesh$shapes)) {
    mesh$shapes[[i]]$indices = mesh$shapes[[i]]$indices[,3:1]
    mesh$shapes[[i]]$norm_indices = mesh$shapes[[i]]$norm_indices[,3:1]
    mesh$shapes[[i]]$tex_indices = mesh$shapes[[i]]$tex_indices[,3:1]
  }
  return(mesh)
}
