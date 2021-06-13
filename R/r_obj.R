#' R 3D Model
#' 
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Return the location of the r_obj() file on your filesystem
#' r_obj()
r_obj = function() {
  system.file("extdata", "r_obj.txt", package="rayvertex")
}
