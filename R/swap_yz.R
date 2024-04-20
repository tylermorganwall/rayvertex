#' Swap Y/Z Axis
#'
#' @param mesh A raymesh object.
#'
#' @return Mesh with Y and Z axis exchanged
#' @export 
#' @examples
#' # Flip a mesh that's originally aligned along the y-axis
#' if(run_documentation()) {
#' cyl_mat = material_list(ambient="red", ambient_intensity=0.3, 
#'                         diffuse="red", diffuse_intensity=0.7)
#' change_material(cylinder_mesh(length = 3, position=c(0,2,0), material = cyl_mat),
#'                 diffuse="green", ambient="green") |> 
#'   add_shape(swap_yz(cylinder_mesh(position=c(0,2,0), length=3, material = cyl_mat))) |> 
#'   rasterize_scene(lookfrom=c(10,10,10), lookat=c(0,0,0), fov=40,
#'                   light_info = directional_light(c(1,1,-1)),
#'                   line_info = generate_line(end=c(10,0,0)) |> 
#'                   add_lines(generate_line(end=c(0,10,0),color="green")) |> 
#'                   add_lines(generate_line(end=c(0,0,10),color="red")))
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
  for(i in seq_along(mesh$normals)) {
    mesh$normals[[i]] = mesh$normals[[i]][,c(1,3,2)]
  }
  return(mesh)
}
