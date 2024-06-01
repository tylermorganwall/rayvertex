#' R 3D Model
#' 
#' 3D obj model of R logo (created from the R SVG logo with the `raybevel` package), 
#' to be used with `obj_model()`
#' 
#' @param simple_r Default `FALSE`. If `TRUE`, this will return a 3D R (instead of the R logo).
#' @return File location of the 3d_r_logo.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Load and render the included example R object file.
#' if(run_documentation()) {
#' obj_mesh(r_obj()) |> 
#'   rasterize_scene(lookfrom = c(0, 1, 10),
#'                   fov=7,light_info = directional_light(c(1,1,1)))
#' }
r_obj = function(simple_r = FALSE) {
  if(!simple_r) {
    system.file("extdata", "3d_r_logo.txt", package="rayvertex")
  } else {
    system.file("extdata", "r_obj.txt", package="rayvertex")
  }
}
