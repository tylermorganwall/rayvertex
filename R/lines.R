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
  start = as.numeric(start)
  end = as.numeric(end)
  
  returnmat = matrix(c(start, end, color), nrow=1,ncol=9)
  colnames(returnmat) = c("x0","y0","z0","x1","y1","z1","r","g","b")
  returnmat
}


#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
rotate_lines = function(lines, angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  angle = angle*pi/180
  lines[,1]  = lines[,1] - pivot_point[1]
  lines[,2]  = lines[,2] - pivot_point[2]
  lines[,3]  = lines[,3] - pivot_point[3]
  lines[,4]  = lines[,4] - pivot_point[1]
  lines[,5]  = lines[,5] - pivot_point[2]
  lines[,6]  = lines[,6] - pivot_point[3]
  rot_mat = generate_rot_matrix(angle, order_rotation)
  
  for(i in seq_len(nrow(lines))) {
    lines[i,1:3] = lines[i,1:3] %*% rot_mat
    lines[i,4:6] = lines[i,4:6] %*% rot_mat
  }
 lines[,1]  = lines[,1] + pivot_point[1]
 lines[,2]  = lines[,2] + pivot_point[2]
 lines[,3]  = lines[,3] + pivot_point[3]
 lines[,4]  = lines[,4] + pivot_point[1]
 lines[,5]  = lines[,5] + pivot_point[2]
 lines[,6]  = lines[,6] + pivot_point[3]
  return(lines)
}

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
scale_lines = function(lines, scale = 1) {
  if(length(scale) == 1) {
    scale = rep(scale,3)
  }
  lines[,1]  = lines[,1] * scale[1]
  lines[,2]  = lines[,2] * scale[2]
  lines[,3]  = lines[,3] * scale[3]
  lines[,4]  = lines[,4] * scale[1]
  lines[,5]  = lines[,5] * scale[2]
  lines[,6]  = lines[,6] * scale[3]
  return(lines)
}

#'@return Rasterized image.
#'@export
#'@examples
translate_lines = function(lines, position = 1) {
  lines[,1]  = lines[,1] + position[1]
  lines[,2]  = lines[,2] + position[2]
  lines[,3]  = lines[,3] + position[3]
  lines[,4]  = lines[,4] + position[1]
  lines[,5]  = lines[,5] + position[2]
  lines[,6]  = lines[,6] + position[3]
  return(lines)
}


#'@title Add light
#'
#'@param obj_model  

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
add_line = function(lines, line) {
  if(nrow(lines) == 0) {
    return(line)
  }
  return(rbind(lines,line))
}