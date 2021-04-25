#'@title Generate Lights
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
point_light = function(position = c(0,0,0), color = "white", constant = 1,
                       falloff = 1, falloff_quad = 1, intensity=1) {
  color = convert_color(color)
  returnmat = matrix(c(position, color*intensity, constant, falloff,falloff_quad), nrow=1,ncol=9)
  colnames(returnmat) = c("x","y","z","r","g","b","constant","falloff","falloff_quad")
  returnmat
}

#'@title Generate Directional Lights
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
directional_light = function(direction = c(0,1,0), color = "white", intensity=1) {
  color = convert_color(color)
  returnmat = matrix(c(direction, color*intensity, 0, 0, 0), nrow=1,ncol=9)
  colnames(returnmat) = c("x","y","z","r","g","b","constant","falloff","falloff_quad")
  returnmat
}

#'@title Add light
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
add_light = function(light_scene, light) {
  rbind(light_scene,light)
}