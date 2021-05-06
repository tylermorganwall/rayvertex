#' R 3D Model
#' 
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Load and render the included example R object file.
r_obj = function() {
  system.file("extdata", "r_obj.txt", package="rayvertex")
}
