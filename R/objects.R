#' Cube 3D Model
#' 
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' #Load and render the included example R object file.
cube_mesh = function(position = c(0,0,0), scale = c(1,1,1), angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
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
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
sphere_mesh = function(position = c(0,0,0), radius = 1, scale = c(1,1,1), angle = c(0,0,0), 
                       pivot_point = c(0,0,0), order_rotation = c(1,2,3),
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
#' 3D obj model of the letter R
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
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
arrow_mesh = function(start = c(0,0,0), end = c(0,1,0), scale = c(1,1,1),  radius_head = 0.5, radius_tail=0.25,
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
    theta = 0
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
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
cylinder_mesh = function(position = c(0,0,0), scale = c(1,1,1), angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  obj = read_obj(system.file("extdata", "cylinder.txt", package="rayvertex"))
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
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
plane_xy_mesh = function(position = c(0,0,0)) {
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
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
plane_xz_mesh = function(position = c(0,0,0), scale = c(1,1,1), angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
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
#' 3D obj model of the letter R
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
plane_yz_mesh = function(position = c(0,0,0), scale = c(1,1,1), angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
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
