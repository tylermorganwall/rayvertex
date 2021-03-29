#'@title Rasterize an OBJ file
#'
#'@param obj_model  A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `c(0,0,0)`. Camera focal position.

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
read_obj = function(filename, materialspath = NULL) {
  filename = path.expand(filename)
  if(is.null(materialspath)) {
    dir = dirname(filename)
  } else {
    dir = materialspath
  }
  lastchar = substr(dir, nchar(dir), nchar(dir))
  fsep = .Platform$file.sep
  if(lastchar!=fsep) {
    dir=paste0(dir,fsep)
  }
  obj_loaded = load_obj(filename, dir)
  obj_loaded
}