#'@title Generate Lines
#'
#'@param start Default `c(0,0,0)`. Start of the line segment.
#'@param end Default `c(0,1,0)`. End of the line segment..
#'@param color Default `white`. Color of the line segment.
#'@return Line matrix
#'@export
#'@examples
#'\dontshow{
#'options("cores"=1)
#'}
#' # Make a spiral of lines
#' t = seq(0,8*pi,length.out=361)
#' line_mat = matrix(nrow=0,ncol=9)
#' 
#' for(i in 1:360) {
#'   line_mat = add_lines(line_mat,
#'                       generate_line(start = c(0.5*sin(t[i]), t[i]/(8*pi), 0.5*cos(t[i])),
#'                                     end  = c(0.5*sin(t[i+1]), t[i+1]/(8*pi), 0.5*cos(t[i+1]))))
#' }
#' rasterize_lines(line_mat)
#' 
#' #Change the line color
#' line_mat = matrix(nrow=0,ncol=9)
#' cols = hsv(seq(0,1,length.out=360))
#' for(i in 1:360) {
#'   line_mat = add_lines(line_mat,
#'                       generate_line(start = c(sin(t[i]), 2*t[i]/(8*pi), cos(t[i])),
#'                                    end  = c(sin(t[i+1]), 2*t[i+1]/(8*pi), cos(t[i+1])),
#'                                    color = cols[i]))
#' }
#' rasterize_lines(line_mat,lookfrom=c(0,10,10),fov=15)
#' 
#' #Use in a scene with a mesh
#' obj_mesh(r_obj(),material=material_list(diffuse="dodgerblue")) %>%
#'  rasterize_scene(line_info = line_mat, light_info = directional_light(c(0,1,1)),
#'                  lookfrom=c(0,5,10),lookat=c(0,0.8,0),fov=15)
generate_line = function(start = c(0,0,0), end = c(0,1,0), color = "white") {
  color = convert_color(color)
  start = as.numeric(start)
  end = as.numeric(end)
  
  returnmat = matrix(c(start, end, color), nrow=1,ncol=9)
  colnames(returnmat) = c("x0","y0","z0","x1","y1","z1","r","g","b")
  returnmat
}

#'@title Rotate Lines
#'
#'@param lines The existing line scene.
#'@param angle Default `c(0,0,0)`. The rotation amount for the x/y/z axes, in degrees.
#'@param pivot_point Default `c(0,0,0)`. The pivot point of the rotation.
#'@param order_rotation Default `c(1,2,3)`. The order in which to perform the rotations.#'
#'@return Rotated lines.
#'
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1)))
#' rasterize_lines(cube_outline,lookfrom=c(0,6,10))
#' 
#' #Rotate the cube 30 degrees around the y-axis
#' rotated_cube = color_lines(rotate_lines(cube_outline,angle=c(0,30,0)),color="red")
#' rasterize_lines(add_lines(cube_outline,rotated_cube),lookfrom=c(0,6,10))
#' 
#' #Rotate the cube 30 degrees around each axis, in this order: x,y,z
#' rotated_cube = color_lines(rotate_lines(cube_outline,angle=c(30,30,30)),color="red")
#' rasterize_lines(add_lines(cube_outline,rotated_cube),lookfrom=c(0,6,10))
#' 
#' #Rotate the cube 30 degrees around each axis, in this order: z,y,x
#' rotated_cube = color_lines(rotate_lines(cube_outline,angle=c(30,30,30), 
#'                            order_rotation = c(3,2,1)),color="red")
#' rasterize_lines(add_lines(cube_outline,rotated_cube),lookfrom=c(0,6,10))
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

#'@title Scale Lines
#'
#'@param lines The line scene.
#'@param scale Default `c(1,1,1)`. The scale amount, per axis. 
#'
#'@return Scaled line matrix.
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1)))
#' rasterize_lines(cube_outline,fov=90,lookfrom=c(0,0,3))
#' 
#' #Scale the cube uniformly
#' scaled_cube = color_lines(scale_lines(cube_outline,scale=0.5),color="red")
#' rasterize_lines(add_lines(cube_outline,scaled_cube),fov=90,lookfrom=c(0,0,3))
#' 
#' #Scale the cube non-uniformly
#' scaled_cube = color_lines(scale_lines(cube_outline,scale=c(0.8,2,0.4)),color="red")
#' rasterize_lines(add_lines(cube_outline,scaled_cube),fov=60,lookfrom=c(3,3,3))
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

#'@title Translate Lines
#'
#'@param lines The line scene.
#'@param position Default `c(0,0,0)`. The translation vector. 
#'
#'@return Translated line matrix.
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1))) 
#' rasterize_lines(cube_outline,fov=40,lookfrom=c(1,2,10),lookat=c(0,0,0))
#' 
#' #Scale the cube uniformly
#' translated_cube = color_lines(translate_lines(cube_outline,c(1,1,1)),"red")
#' translated_cube2 = color_lines(translate_lines(cube_outline,c(-1,-1,-1)),"green")
#' 
#' cube_outline %>% 
#'   add_lines(translated_cube) %>% 
#'   add_lines(translated_cube2) %>% 
#'   rasterize_lines(fov=40,lookfrom=c(1,2,10),lookat=c(0,0,0))
translate_lines = function(lines, position = 1) {
  lines[,1]  = lines[,1] + position[1]
  lines[,2]  = lines[,2] + position[2]
  lines[,3]  = lines[,3] + position[3]
  lines[,4]  = lines[,4] + position[1]
  lines[,5]  = lines[,5] + position[2]
  lines[,6]  = lines[,6] + position[3]
  return(lines)
}

#'@title Color Lines
#'
#'@param lines The line scene.
#'@param color Default `white`. The color to convert the lines to.
#'
#'@return Colored line matrix.
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1)))
#'   
#' cube_outline %>% 
#'   color_lines(color="red") %>% 
#'   rasterize_lines()
color_lines = function(lines, color = "white") {
  color = convert_color(color)
  lines[,7]  = color[1]
  lines[,8]  = color[2]
  lines[,9]  = color[3]
  return(lines)
}


#'@title Add Line
#'
#'@param lines Existing lines or empty (0-row) matrix.
#'@param line Line to add, generated with `generate_line()`
#'
#'@return New line matrix.
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1)))
#'   
#' rasterize_lines(cube_outline,fov=90,lookfrom=c(0,0,3))
add_lines = function(lines, line) {
  if(nrow(lines) == 0) {
    return(line)
  }
  return(rbind(lines,line))
}
