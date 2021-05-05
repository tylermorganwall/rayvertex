#'@title Generate Lines
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
generate_line = function(start = c(0,0,0), end = c(0,1,0), color = "white") {
  color = convert_color(color)
  returnmat = matrix(c(start, end, color), nrow=1,ncol=9)
  colnames(returnmat) = c("x0","y0","z0","x1","y1","z1","r","g","b")
  returnmat
}


#'@title Add light
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
add_line = function(lines, line) {
  rbind(lines,line)
}