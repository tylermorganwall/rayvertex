#' Flip Orientation
#'
#' @param mesh The mesh to swap orientations.
#' 
#' @return Mesh with flipped vertex orientation
#' @export 
#' @examples
#' # Flip a mesh 
#' sphere_mesh(position=c(-1,0,0)) |> 
#'   add_shape(flip_orientation_mesh(sphere_mesh(position=c(1,0,0)))) |> 
#'   rasterize_scene(debug="normals",fov=30)
flip_orientation_mesh = function(mesh) {
  for(i in seq_along(mesh$shapes)) {
    mesh$shapes[[i]]$indices = mesh$shapes[[i]]$indices[,3:1]
    mesh$shapes[[i]]$norm_indices = mesh$shapes[[i]]$norm_indices[,3:1]
    mesh$shapes[[i]]$tex_indices = mesh$shapes[[i]]$tex_indices[,3:1]
  }
  return(mesh)
}