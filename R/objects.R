#' Cube 3D Model
#' 
#' 3D obj model of the letter R
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate a cube
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), scale = 100)) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a blue rotated cube 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), scale = 100, angle=c(0,45,0),
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a scaled, blue rotated cube 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), angle=c(0,45,0),
#'                       scale = c(2,0.5,0.8)*100,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
cube_mesh = function(position = c(0,0,0), 
                     scale = c(1,1,1), 
                     angle = c(0,0,0), 
                     pivot_point = c(0,0,0), 
                     order_rotation = c(1,2,3),
                     material = material_list()) {
  obj = read_obj(system.file("extdata", "cube.txt", package="rayvertex"))
  obj = set_material(obj, material = material)
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj = add_shape(obj,obj2)
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
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate a sphere in the cornell box.
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(sphere_mesh(position = c(555/2, 555/2, 555/2), radius = 100)) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' 
#' #Generate a shiny sphere in the cornell box
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(sphere_mesh(position = c(555/2, 100, 555/2), radius = 100, 
#'                     material = material_list(diffuse = "gold",type="phong"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate an ellipsoid in the cornell box
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(sphere_mesh(position = c(555/2, 210, 555/2), radius = 100, 
#'                         angle=c(0,30,0), scale = c(0.5,2,0.5),
#'                         material = material_list(diffuse = "dodgerblue",type="phong"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
sphere_mesh = function(position = c(0,0,0), 
                       scale = c(1,1,1), 
                       angle = c(0,0,0), 
                       pivot_point = c(0,0,0), 
                       order_rotation = c(1,2,3),
                       radius = 1, 
                       low_poly = FALSE,
                       material = material_list()) {
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
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj2 = rotate_mesh(obj2, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
    obj2 = translate_mesh(obj2,position)
  }
  obj = translate_mesh(obj,position)
  obj = set_material(obj, material = material)
  if(material$type == "toon" || material$type == "toon_phong") {
    obj = add_shape(obj,obj2)
  }
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
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate a cone
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2),
#'                       radius = 100)) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a blue cone with a wide base
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius=200,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a long, thin cone
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 400, 555/2), radius=50,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
cone_mesh = function(start = c(0,0,0), end=c(0,1,0), 
                     radius = 0.5, direction = NA, from_center = FALSE,
                     material = material_list()) {
  obj = read_obj(system.file("extdata", "cone.txt", package="rayvertex"))
  obj = set_material(obj, material = material)
  
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
      theta = 180
    } else {
      theta = 0
    }
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, -phi, theta)
  
  obj = scale_mesh(obj, scale = c(radius/0.5,fulllength,radius/0.5))
  
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj = add_shape(obj,obj2)
  }
  
  obj = rotate_mesh(obj, angle=angle, order_rotation=order_rotation)
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' Arrow 3D Model
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
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#'
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate an arrow
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius_tail=50,
#'                        radius_top = 100,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a blue arrow with a wide tail
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius_tail=100,
#'                        radius_top = 150,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a long, thin arrow and change the proportions
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 400, 555/2), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Change the start and end points
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(arrow_mesh(start = c(500, 20, 555/2), end = c(50, 500, 555/2), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="dodgerblue"))) %>%
#'   add_shape(arrow_mesh(start = c(500, 500, 500), end = c(50, 50, 50), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="red"))) %>%
#'   add_shape(arrow_mesh(start = c(555/2, 50, 500), end = c(555/2, 50, 50), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="green"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
arrow_mesh = function(start = c(0,0,0), end = c(0,1,0), radius_top = 0.5, radius_tail=0.25,
                      tail_proportion = 0.5,
                      direction = NA,  from_center = TRUE,
                      material = material_list()) {
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
      theta = 180
    } else {
      theta = 0
    }
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, -phi, theta)
  
  obj = read_obj(system.file("extdata", "arrow.txt", package="rayvertex"))
  obj = set_material(obj, material = material)
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    
    new_radius = (2*radius_top) / (fulllength * (1-tail_proportion)) * (fulllength * (1-tail_proportion) + material$toon_outline_width)
    
    obj2$vertices[c(1:32,66:97),c(1,3)] = obj$vertices[c(1:32,66:97),c(1,3)] * radius_tail/0.25 * (radius_tail + material$toon_outline_width/2)/radius_tail
    obj2$vertices[34:65,c(1,3)] = obj$vertices[34:65,c(1,3)] * new_radius

    #Proportions
    obj2$vertices[33,2]            = (1 - 0.5) * fulllength + material$toon_outline_width/2
    obj2$vertices[c(1:32,34:65),2] = (tail_proportion  - 0.5) * fulllength - material$toon_outline_width/2
    obj2$vertices[66:97,2]         = (-0.5 * fulllength) - material$toon_outline_width/2
    
    obj = add_shape(obj,obj2)
  }
  
  obj$vertices[c(1:32,66:97),c(1,3)] = obj$vertices[c(1:32,66:97),c(1,3)] * radius_tail/0.25
  obj$vertices[34:65,c(1,3)] = obj$vertices[34:65,c(1,3)] * radius_top/0.5
  
  #Proportions
  obj$vertices[33,2]            = (1 - 0.5) * fulllength
  obj$vertices[c(1:32,34:65),2] = (tail_proportion  - 0.5) * fulllength
  obj$vertices[66:97,2]         = (-0.5 * fulllength)

  obj = rotate_mesh(obj, angle=angle, order_rotation=order_rotation)
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' Cylinder 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param radius Default `0.5`. Radius of the cylinder.
#' @param length Default `1`. Length of the cylinder.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate a cylinder
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cylinder_mesh(position=c(555/2,150,555/2),
#'                           radius = 50, length=300, material = material_list(diffuse="purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a wide, thin disk
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cylinder_mesh(position=c(555/2,20,555/2),
#'                           radius = 200, length=5, material = material_list(diffuse="purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' 
#' #Generate a narrow cylinder
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(cylinder_mesh(position=c(555/2,555/2,555/2),angle=c(45,-45,0),
#'                           radius = 10, length=500, material = material_list(diffuse="purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
cylinder_mesh = function(position = c(0,0,0), radius = 0.5, length=1,
                         angle = c(0,0,0), 
                         pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                         material = material_list()) {
  obj = read_obj(system.file("extdata", "cylinder.txt", package="rayvertex"))
  obj = set_material(obj, material = material)
  obj = scale_mesh(obj, scale=c(radius,length,radius))

  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj = add_shape(obj,obj2)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)

  obj
}

#' Segment 3D Model
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
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' #Generate a segment in the cornell box. 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(455, 455, 455), radius = 50)) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#'
#' # Draw a line graph representing a normal distribution, but with metal:
#' xvals = seq(-3, 3, length.out = 30)
#' yvals = dnorm(xvals)
#' 
#' scene_list = list()
#' for(i in 1:(length(xvals) - 1)) {
#'   scene_list = add_shape(scene_list, 
#'                          segment_mesh(start = c(555/2 + xvals[i] * 80, yvals[i] * 800, 555/2),
#'                             end = c(555/2 + xvals[i + 1] * 80, yvals[i + 1] * 800, 555/2),
#'                             radius = 10,
#'                             material = material_list(diffuse="purple", type="phong")))
#' }
#' \donttest{
#' generate_cornell_mesh() %>% 
#'   add_shape(scene_list) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#'
#' #Draw the outline of a cube:
#' 
#' cube_outline = segment_mesh(start = c(100, 100, 100), end = c(100, 100, 455), radius = 10) %>%
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(100, 455, 100), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(455, 100, 100), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 100, 455), end = c(100, 455, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 100, 455), end = c(455, 100, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 455, 455), end = c(100, 455, 100), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 455, 455), end = c(455, 455, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(455, 455, 100), end = c(455, 100, 100), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(455, 455, 100), end = c(455, 455, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(455, 100, 100), end = c(455, 100, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(455, 100, 455), end = c(455, 455, 455), radius = 10)) %>%
#'   add_shape(segment_mesh(start = c(100, 455, 100), end = c(455, 455, 100), radius = 10))
#' 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(set_material(cube_outline,diffuse="dodgerblue",type="phong")) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' 
#' #Shrink and rotate the cube
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(
#'     scale_mesh(rotate_mesh(set_material(cube_outline,diffuse="dodgerblue",type="phong"),
#'                 angle=c(45,45,45), pivot_point=c(555/2,555/2,555/2)),0.5,
#'                 center=c(555/2,555/2,555/2))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
segment_mesh = function(start = c(0,-1,0), end = c(0,1,0), radius = 0.5,
                        direction = NA,  from_center = TRUE, square = FALSE,
                        material = material_list()) {
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
  phi =  atan2( as.numeric(end[1]-start[1]), as.numeric(end[3]-start[3]))/pi*180+90
  
  length_xy = sqrt((end[1]-start[1])^2 + (end[3]-start[3])^2)
  if(end[1] == start[1] && end[3] == start[3]) {
    if(start[2] - end[2] > 0) {
      theta = 180
    } else {
      theta = 0
    }
  } else {
    theta = atan2(-length_xy, (end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, -phi, theta)
  
  if(!square) {
    obj = cylinder_mesh(angle = angle, order_rotation = order_rotation, radius = radius, length = fulllength, material = material)
  } else {
    obj = cube_mesh(angle = angle, order_rotation = order_rotation, scale=c(radius*2,fulllength,radius*2), material = material)
  }
  obj = translate_mesh(obj,c(x,y,z))
  obj
}

#' XY Rectangle 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(xy_rect_mesh(position = c(555/2, 100, 555/2), scale=200,
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' 
#' #Rotate the plane and scale 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(xy_rect_mesh(position = c(555/2, 100, 555/2), scale=c(200,100,1), angle=c(0,30,0),
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
xy_rect_mesh = function(position = c(0,0,0), 
                        scale = c(1,1,1), 
                        angle = c(0,0,0), 
                        pivot_point = c(0,0,0), 
                        order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = read_obj(system.file("extdata", "xy_plane.txt", package="rayvertex"))
  obj = set_material(obj, material = material)
  
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj = add_shape(obj,obj2)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' XZ Rectangle 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(xz_rect_mesh(position = c(555/2, 100, 555/2), scale=200,
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' 
#' #Rotate the plane and scale 
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(xz_rect_mesh(position = c(555/2, 100, 555/2), scale=c(200,1,100), angle=c(0,30,0),
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
xz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                         angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = read_obj(system.file("extdata", "xz_plane.txt", package="rayvertex"))
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj = set_material(obj, material = material)
  obj
}

#' YZ Rectangle 3D Model
#' 
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return File location of the R.obj file (saved with a .txt extension)
#' @export
#' @examples
#' \dontshow{
#' options("cores"=1)
#' }
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(yz_rect_mesh(position = c(500, 100, 555/2), scale=200,
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' 
#' #Need to flip it around to see the other side
#' \donttest{
#' generate_cornell_mesh() %>%
#'   add_shape(yz_rect_mesh(position = c(100, 100, 555/2), scale=c(1,200,200), angle=c(0,180,0),
#'              material = material_list(diffuse = "purple"))) %>%
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
yz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                        angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = read_obj(system.file("extdata", "yz_plane.txt", package="rayvertex"))

  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    obj = add_shape(obj,obj2)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj = set_material(obj, material = material, id = 1)
  obj
}

#' Cornell Box 3D Model
#'
#' @param leftcolor Default `#1f7326` (green).
#' @param rightcolor Default `#a60d0d` (red).
#' @param roomcolor Default `#bababa` (light grey).
#' @param ceiling Default `TRUE`. Whether to render the ceiling.
#' @param light Default `TRUE`. Whether to render a point light near the ceiling.
#'@return Rasterized image.
#'@export
#'@examples
#'\dontshow{
#' options("cores"=1)
#' }
#' #Generate and render the default Cornell box and add an object.
#' \donttest{
#' generate_cornell_mesh() %>% 
#'   rasterize_scene()
#' }
#' #Add an object to the scene
#'\donttest{
#' generate_cornell_mesh() %>% 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,0,555/2),scale=150,angle=c(0,180,0))) %>% 
#'   rasterize_scene()
#'}
#' #Turn off the ceiling so the default directional light reaches inside the box
#'\donttest{
#' generate_cornell_mesh(ceiling=FALSE) %>% 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,0,555/2),scale=150,angle=c(0,180,0))) %>% 
#'   rasterize_scene()
#'}
#' #Adjust the light to the front
#' \donttest{
#' generate_cornell_mesh(ceiling=FALSE) %>% 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,0,555/2),scale=150,angle=c(0,180,0))) %>% 
#'   rasterize_scene(light_info = directional_light(direction=c(0,1,-1)))
#' }
#' #Change the color palette
#'\donttest{
#' generate_cornell_mesh(ceiling=FALSE,leftcolor="purple", rightcolor="yellow") %>% 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,0,555/2),scale=150,angle=c(0,180,0))) %>% 
#'   rasterize_scene(light_info = directional_light(direction=c(0,1,-1)))
#'}
generate_cornell_mesh = function(leftcolor = "#1f7326", 
                                 rightcolor = "#a60d0d", roomcolor = "#bababa", ceiling = TRUE,
                                 light = TRUE) {
  ambient_intensity = 0.25
  if(ceiling) {
    scene = set_material(cube_mesh(position=c(555,555/2+2.5,555/2),scale=c(10,550,555)),
                         diffuse=leftcolor,ambient = leftcolor, ambient_intensity=ambient_intensity) %>%
      add_shape(set_material(cube_mesh(position=c(0,555/2+2.5,555/2),angle=c(0,180,0),scale=c(10,555,555)),
                             diffuse=rightcolor,ambient = rightcolor, ambient_intensity=ambient_intensity)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,555+5,555/2),scale=c(560,10,560),angle=c(0,0,180)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,0,555/2),scale=c(560,10,560)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) %>% 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2,555),scale=c(560,555,10)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity))
  } else {
    scene = set_material(cube_mesh(position=c(555,555/2+2.5,555/2),scale=c(10,550,555)),
                         diffuse=leftcolor,ambient = leftcolor, ambient_intensity=ambient_intensity) %>%
      add_shape(set_material(cube_mesh(position=c(0,555/2+2.5,555/2),angle=c(0,180,0),scale=c(10,550,555)),
                             diffuse=rightcolor,ambient = rightcolor, ambient_intensity=ambient_intensity)) %>%
      add_shape(set_material(cube_mesh(position=c(555/2,0,555/2),scale=c(560,10,560)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) %>% 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2,555),scale=c(560,555,10)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity))
  }
  attr(scene,"cornell") = TRUE
  attr(scene,"cornell_light") = light
  scene
}

#' OBJ Mesh 3D Model
#'
#' @param filename OBJ filename.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#' @param material Default `NULL`, read from the MTL file. If not `NULL`, this accepts the output
#' from the `material_list()` function to specify the material.
#'@return Rasterized image.
#'@export
#'@examples
#'\dontshow{
#' options("cores"=1)
#' }
#' #Read in the provided 3D R mesh
#' generate_cornell_mesh(ceiling=FALSE) %>% 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,0,555/2),scale=150,angle=c(0,180,0))) %>% 
#'   rasterize_scene(light_info = directional_light(direction=c(0.2,0.5,-1)))
obj_mesh = function(filename, position = c(0,0,0), scale = c(1,1,1), 
                    angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), materialspath = NULL,
                    material = NULL) {
  obj_loaded = read_obj(filename, materialspath)
  if(any(scale != 1)) {
    obj_loaded = scale_mesh(obj_loaded, scale=scale)
  }
  if(!is.null(material)) {
    obj_loaded = set_material(obj_loaded,material = material)
    if(material$type == "toon" || material$type == "toon_phong") {
      obj2 = generate_toon_outline(obj_loaded, material)
      obj_loaded = add_shape(obj_loaded,obj2)
    }
  }
  if(any(angle != 0)) {
    obj_loaded = rotate_mesh(obj_loaded, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  
  obj_loaded = translate_mesh(obj_loaded,position)
  obj_loaded
}

#' Torus 3D Model
#'
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param radius Default `0.5`. The radius of the torus.
#' @param ring_radius Default `0.2`. The radius of the ring.
#' @param sides Default `36`. The number of faces around the ring when triangulating the torus.
#' @param rings Default `36`. The number of faces around the torus.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#'@return Torus mesh
#'@export
#'@examples
#'\dontshow{
#' options("cores"=1)
#' }
#'#Plot a group of tori in the cornell box
#'generate_cornell_mesh(ceiling = FALSE) %>% 
#'  add_shape(torus_mesh(position=c(555/2,555/3,555/2), angle=c(20,0,45),
#'                       radius=120, ring_radius = 40,
#'                       material = material_list(diffuse="dodgerblue4",type="phong",
#'                                                ambient="dodgerblue4",ambient_intensity=0.2))) %>%
#'  add_shape(torus_mesh(position=c(400,400,555/2), angle=c(20,200,45),radius=80, ring_radius = 30,
#'                       material=material_list(diffuse="orange",type="phong",
#'                                              ambient="orange",ambient_intensity=0.2))) %>%
#'  add_shape(torus_mesh(position=c(150,450,555/2), angle=c(60,180,0),radius=40, ring_radius = 20,
#'                       material=material_list(diffuse="red",type="phong"))) %>%
#'  rasterize_scene(light_info = directional_light(c(0,1,-2)))
torus_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                      angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                      radius = 0.5, ring_radius = 0.2, sides = 36, rings=36,
                      material = material_list()) {
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
  normals = do.call(rbind,normals)
  normalize = function(x) {
    x/sqrt(sum(x * x))
  }
  normalized_normals = t(apply(normals,1,normalize))
  obj = construct_mesh(vertices = do.call(rbind,vertices),
                       indices = indices,
                       normals = normalized_normals,
                       norm_indices = indices,
                       material = material)
  
  if(any(scale != 1)) {
    obj = scale_mesh(obj, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = torus_mesh(position = c(0,0,0), scale = scale, 
                      radius = radius, 
                      ring_radius = ring_radius + material$toon_outline_width/2 , sides = sides, rings=rings,
                      material = material_list(diffuse=material$toon_outline_color, type="color",culling="front"))
    obj = add_shape(obj,obj2)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' OBJ Mesh 3D Model
#'
#' @param mesh Mesh3d object.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#' @param material Default `NULL`, read from the MTL file. If not `NULL`, this accepts the output
#' from the `material_list()` function to specify the material.
#'@return Rasterized image.
#'@export
#'@examples
#'\dontshow{
#' options("cores"=1)
#' }
#' #Read in a mesh3d object and rasterize it
#' if("Rvcg" %in% rownames(utils::installed.packages())) {
#'   library(Rvcg)
#'   data(humface)
#'   
#'   mesh3d_mesh(humface,position = c(0,-0.3,0),scale = 1/70,
#'               material=material_list(diffuse="dodgerblue4", type="phong", shininess=20,
#'               ambient = "dodgerblue4", ambient_intensity=0.3)) %>%
#'     rasterize_scene(lookat = c(0,0.5,1), light_info = directional_light(c(1,0.5,1)))
#' }
mesh3d_mesh = function(mesh, position = c(0,0,0), scale = c(1,1,1), 
                       angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), materialspath = NULL,
                       material = material_list()) {
  mat_vals = mesh$material
  if(!is.null(mat_vals)) {
    if(!is.null(mat_vals$color)) {
      diffuse_val = mat_vals$color
    } else {
      diffuse_val = material$diffuse
    }
    if(!is.null(mat_vals$alpha)) {
      dissolve_val = mat_vals$alpha
    } else {
      dissolve_val = material$dissolve
    }
    if(!is.null(mat_vals$ambient)) {
      ambient_val = mat_vals$ambient
    } else {
      ambient_val = material$ambient
    }
    if(!is.null(mat_vals$shininess)) {
      exponent_val = mat_vals$shininess
    } else {
      exponent_val = material$shininess
    }
    mesh = construct_mesh(vertices = t(mesh$vb)[,1:3], 
                          indices = t(mesh$it)-1,
                          material = material_list(
                            diffuse = diffuse_val,
                            dissolve = dissolve_val,
                            ambient =  ambient_val,
                            shininess = exponent_val))
  } else {
    mesh = construct_mesh(vertices = t(mesh$vb)[,1:3], 
                          indices = t(mesh$it)-1,
                          material = material)
  }

  if(any(scale != 1)) {
    mesh = scale_mesh(mesh, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(mesh, material)
    mesh = add_shape(mesh,obj2)
  }
  if(any(angle != 0)) {
    mesh = rotate_mesh(mesh, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  mesh = translate_mesh(mesh,position)
  mesh
}
