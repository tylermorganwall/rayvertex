#' Cube 3D Model
#' 
#' 3D obj model of the letter R
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Load and render the included example R object file.
cube_mesh = function(position = c(0,0,0), 
                     scale = c(1,1,1), 
                     angle = c(0,0,0), 
                     pivot_point = c(0,0,0), 
                     order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "cube.txt", package="rayvertex"))
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Sphere 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param radius Default `1`. Radius of the sphere.
#' @param low_poly Default `FALSE`. If `TRUE`, will use a low-poly sphere.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
sphere_mesh = function(position = c(0,0,0), 
                       scale = c(1,1,1), 
                       angle = c(0,0,0), 
                       pivot_point = c(0,0,0), 
                       order_rotation = c(1,2,3),
                       radius = 1, 
                       low_poly = FALSE) {
  if(!low_poly) {
    obj = read_obj(system.file("extdata", "sphere.txt", package="rayvertex"))
  } else {
    obj = read_obj(system.file("extdata", "low_poly_sphere.txt", package="rayvertex"))
  }
  obj$vertices = obj$vertices * radius
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Cone 3D Model
#' 
#' @param start Default `c(0, 0, 0)`. Base of the cone, specifying `x`, `y`, `z`.
#' @param end Default `c(0, 1, 0)`. Tip of the cone, specifying `x`, `y`, `z`.
#' @param radius Default `1`. Radius of the bottom of the cone.
#' @param direction Default `NA`. Alternative to `start` and `end`, specify the direction (via 
#' a length-3 vector) of the cone. Cone will be centered at `start`, and the length will be
#' determined by the magnitude of the direction vector.
#' @param from_center Default `TRUE`. If orientation specified via `direction`, setting this argument
#' to `FALSE` will make `start` specify the bottom of the cone, instead of the middle.
#' 
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
cone_mesh = function(start = c(0,0,0), end=c(0,1,0), 
                     radius = 0.5, direction = NA, from_center = FALSE) {
  obj = read_obj(system.file("extdata", "cone.txt", package="rayvertex"))
  if(all(!is.na(direction)) && length(direction) == 3) {
    if(from_center) {
      new_start = start - direction/2
      new_end = start + direction/2
    } else {
      new_start = start
      new_end = start + direction
    }
    start = new_start
    end = new_end
  } 
  x = (start[1] + end[1])/2
  y = (start[2] + end[2])/2
  z = (start[3] + end[3])/2
  order_rotation = c(3, 2, 1)
  phi =  atan2( as.numeric(end[1]-start[1]), as.numeric(end[3]-start[3]))/pi*180 + 90
  
  length_xy = sqrt((end[1]-start[1])^2 + (end[3]-start[3])^2)
  if(end[1] == start[1] && end[3] == start[3]) {
    theta = 0
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, phi, theta)
  
  obj = scale_mesh(obj, scale = c(radius/0.5,fulllength,radius/0.5))
  obj = rotate_mesh(obj, angle=angle, order_rotation=order_rotation)
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' Cube 3D Model
#' 
#' @param start Default `c(0, 0, 0)`. Base of the arrow, specifying `x`, `y`, `z`.
#' @param end Default `c(0, 1, 0)`. Tip of the arrow, specifying `x`, `y`, `z`.
#' @param radius_top Default `0.5`. Radius of the top of the arrow.
#' @param radius_tail Default `0.2`.  Radius of the tail of the arrow.
#' @param tail_proportion Default `0.5`. Proportion of the arrow that is the tail.
#' @param direction Default `NA`. Alternative to `start` and `end`, specify the direction (via 
#' a length-3 vector) of the arrow. Arrow will be centered at `start`, and the length will be
#' determined by the magnitude of the direction vector.
#' @param from_center Default `TRUE`. If orientation specified via `direction`, setting this argument
#' to `FALSE` will make `start` specify the bottom of the cone, instead of the middle.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
arrow_mesh = function(start = c(0,0,0), end = c(0,1,0), radius_top = 0.5, radius_tail=0.25,
                      tail_proportion = 0.5,
                      direction = NA,  from_center = TRUE) {
  stopifnot(tail_proportion > 0 && tail_proportion < 1)
  
  if(all(!is.na(direction)) && length(direction) == 3) {
    if(from_center) {
      new_start = start - direction/2
      new_end = start + direction/2
    } else {
      new_start = start
      new_end = start + direction
    }
    start = new_start
    end = new_end
  } 
  x = (start[1] + end[1])/2
  y = (start[2] + end[2])/2
  z = (start[3] + end[3])/2
  order_rotation = c(3, 2, 1)
  phi =  atan2( as.numeric(end[1]-start[1]), as.numeric(end[3]-start[3]))/pi*180 + 90
  
  length_xy = sqrt((end[1]-start[1])^2 + (end[3]-start[3])^2)
  if(end[1] == start[1] && end[3] == start[3]) {
    if(start[2] - end[2] > 0) {
      theta = 0
    } else {
      theta = 180
    }
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, phi, theta)
  
  obj = read_obj(system.file("extdata", "arrow.txt", package="rayvertex"))
  obj$vertices[c(1:32,66:97),c(1,3)] = obj$vertices[c(1:32,66:97),c(1,3)] * radius_tail/0.25
  obj$vertices[34:65,c(1,3)] = obj$vertices[34:65,c(1,3)] * radius_head/0.5
  
  #Proportions
  obj$vertices[33,2]            = (1 - 0.5) * fulllength
  obj$vertices[c(1:32,34:65),2] = (tail_proportion  - 0.5) * fulllength
  obj$vertices[66:97,2]         = (-0.5 * fulllength)
  obj = rotate_mesh(obj, angle=angle, order_rotation=order_rotation)
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' Cube 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param radius Default `0.5`. Radius of the cylinder.
#' @param length Default `1`. Length of the cylinder.
#' 
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
cylinder_mesh = function(position = c(0,0,0), radius = 0.5, length=1,
                         angle = c(0,0,0), 
                         pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "cylinder.txt", package="rayvertex"))
  obj = scale_mesh(obj, scale=c(radius,length,radius))
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Cube 3D Model
#' 
#' @param start Default `c(0, 0, 0)`. Base of the segment, specifying `x`, `y`, `z`.
#' @param end Default `c(0, 1, 0)`. End of the segment, specifying `x`, `y`, `z`.
#' @param radius Default `0.5`. Radius of the cylinder.
#' @param direction Default `NA`. Alternative to `start` and `end`, specify the direction (via 
#' a length-3 vector) of the arrow. Arrow will be centered at `start`, and the length will be
#' determined by the magnitude of the direction vector.
#' @param from_center Default `TRUE`. If orientation specified via `direction`, setting this argument
#' to `FALSE` will make `start` specify the bottom of the cone, instead of the middle.
#' @param square Default `FALSE`. If `TRUE`, will use a square instead of a circle for the cylinder.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
segment_mesh = function(start = c(0,-1,0), end = c(0,1,0), radius = 0.5,
                        direction = NA,  from_center = TRUE, square = FALSE) {
  if(all(!is.na(direction)) && length(direction) == 3) {
    if(from_center) {
      new_start = start - direction/2
      new_end = start + direction/2
    } else {
      new_start = start
      new_end = start + direction
    }
    start = new_start
    end = new_end
  } 
  x = (start[1] + end[1])/2
  y = (start[2] + end[2])/2
  z = (start[3] + end[3])/2
  order_rotation = c(3, 2, 1)
  phi =  atan2( as.numeric(end[1]-start[1]), as.numeric(end[3]-start[3]))/pi*180 + 90
  
  length_xy = sqrt((end[1]-start[1])^2 + (end[3]-start[3])^2)
  if(end[1] == start[1] && end[3] == start[3]) {
    if(start[2] - end[2] > 0) {
      theta = 0
    } else {
      theta = 180
    }
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, phi, theta)
  
  if(!square) {
    obj = cylinder_mesh(angle = angle, order_rotation = order_rotation, radius = radius, length = fulllength)
  } else {
    obj = cube_mesh(angle = angle, order_rotation = order_rotation, scale=c(radius*2,fulllength,radius*2))
  }
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' Cube 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
xy_rect_mesh = function(position = c(0,0,0), 
                         scale = c(1,1,1), 
                         angle = c(0,0,0), 
                         pivot_point = c(0,0,0), 
                         order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "xy_plane.txt", package="rayvertex"))
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Cube 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
xz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                         angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "xz_plane.txt", package="rayvertex"))
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Cube 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
yz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                        angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "yz_plane.txt", package="rayvertex"))
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#'@title Rasterize an OBJ file
#'
#' @param leftcolor Default `#1f7326` (green).
#' @param rightcolor Default `#a60d0d` (red).
#' @param roomcolor Default `#bababa` (light grey).
#' @param ceiling Default `TRUE`. Whether to render the ceiling.
#' @param light Default `TRUE`. Whether to render a point light near the ceiling.
#' @param simulate_diffuse_light Default `TRUE`. Whether place point lights in the scene to 
#' simulate the first diffuse bounce of light.
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
generate_cornell_mesh = function(leftcolor = "#1f7326", 
                                 rightcolor = "#a60d0d", roomcolor = "#bababa", ceiling = TRUE,
                                 light = TRUE, simulate_diffuse_light = TRUE) {
  if(ceiling) {
    scene = set_material(cube_mesh(position=c(555,555/2+2.5,555/2),scale=c(10,550,555)),diffuse=leftcolor) %>%
      add_shape(set_material(cube_mesh(position=c(0,555/2+2.5,555/2),angle=c(0,180,0),scale=c(10,555,555)),diffuse=rightcolor)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,555+5,555/2),scale=c(560,10,560),angle=c(0,0,180)),diffuse=roomcolor)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,0,555/2),scale=c(560,10,560)),diffuse=roomcolor)) %>% 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2,555),scale=c(560,555,10)),diffuse=roomcolor))
  } else {
    scene = set_material(cube_mesh(position=c(555,555/2+2.5,555/2),scale=c(10,550,555)),diffuse=leftcolor) %>%
      add_shape(set_material(cube_mesh(position=c(0,555/2+2.5,555/2),angle=c(0,180,0),scale=c(10,550,555)),diffuse=rightcolor)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,0,555/2),scale=c(560,10,560)),diffuse=roomcolor)) %>% 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2,555),scale=c(560,555,10)),diffuse=roomcolor))
  }
  attr(scene,"cornell") = TRUE
  attr(scene,"cornell_light") = light
  attr(scene,"cornell_diffuse_light") = simulate_diffuse_light
  
  scene
}

#'@title Rasterize an OBJ file
#'
#' @param filename OBJ filename.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
obj_mesh = function(filename, position = c(0,0,0), scale = c(1,1,1), 
                    angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), materialspath = NULL) {
  obj_loaded = read_obj(filename, materialspath)
  if(any(scale != 1)) {
    obj_loaded = scale_mesh(obj_loaded, scale=scale)
  }
  if(any(angle != 0)) {
    obj_loaded = rotate_mesh(obj_loaded, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj_loaded = translate_mesh(obj_loaded,position)
  obj_loaded
}

#'@title Rasterize an OBJ file
#'
#' @param filename OBJ filename.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#'@return Rasterized image.
#'@export
#'@examples
torus_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                      angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                      radius = 0.5, ring_radius = 0.2, sides = 36, rings=36) {
  num_vertices_row = sides + 1
  num_vertices_col = rings + 1
  numVertices = num_vertices_row * num_vertices_col
  theta = 0.0
  phi = 0.0
  
  vertical_stride   = pi * 2.0 / rings
  horizontal_stride = pi * 2.0 / sides 
  
  counter = 1
  vertices = list()
  for (v_iter in seq_len(num_vertices_col)-1) {
    theta = vertical_stride * v_iter
    for (h_iter in seq_len(num_vertices_row)-1) {
      phi = horizontal_stride * h_iter
      
      x = cos(theta) * (radius + ring_radius * cos(phi))
      y = sin(theta) * (radius + ring_radius * cos(phi))
      z = ring_radius *  sin(phi)
      
      vertices[[counter]] = matrix(c(x,z,y),ncol=3)
      counter = counter + 1
    }
  }
  
  counter = 1
  normals = list()
  for (v_iter in seq_len(num_vertices_col)-1) {
    theta = vertical_stride * v_iter
    for (h_iter in seq_len(num_vertices_row)-1) {
      phi = horizontal_stride * h_iter
      
      x = cos(theta) * (ring_radius * cos(phi))
      y = sin(theta) * (ring_radius * cos(phi))
      z = ring_radius *  sin(phi)
      
      normals[[counter]] = matrix(c(x,z,y),ncol=3)
      counter = counter + 1
    }
  }
  
  indices = list()
  counter = 1
  for (v_iter in seq_len(rings)-1) {
    for (h_iter in seq_len(sides)-1) {
      lt = (h_iter + v_iter * (num_vertices_row))
      rt = (h_iter + 1) + v_iter * (num_vertices_row)
      lb = (h_iter + (v_iter + 1) * (num_vertices_row))
      rb = (h_iter + 1) + (v_iter + 1) * (num_vertices_row)
      indices[[counter]] = matrix(c(lt,rt,lb),ncol=3);
      counter = counter + 1
      indices[[counter]] = matrix(c(rt,rb,lb),ncol=3);
      counter = counter + 1
    }
  }
  indices = do.call(rbind,indices)
  obj = construct_mesh(vertices = do.call(rbind,vertices),
                       indices = indices,
                       normals = do.call(rbind,normals),
                       norm_indices = indices)
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}



