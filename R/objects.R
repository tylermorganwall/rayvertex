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
#' @return List describing the mesh.
#' @export
#'
#' @examples
#' if(run_documentation()) {
#' #Generate a cube
#' generate_cornell_mesh() |>
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), scale = 100)) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a blue rotated cube 
#' generate_cornell_mesh() |>
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), scale = 100, angle=c(0,45,0),
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a scaled, blue rotated cube 
#' generate_cornell_mesh() |>
#'   add_shape(cube_mesh(position = c(555/2, 100, 555/2), angle=c(0,45,0),
#'                       scale = c(2,0.5,0.8)*100,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'}
cube_mesh = function(position = c(0,0,0), 
                     scale = c(1,1,1), 
                     angle = c(0,0,0), 
                     pivot_point = c(0,0,0), 
                     order_rotation = c(1,2,3),
                     material = material_list()) {
  obj = get("cube", envir = ray_environment)
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
#' @param normals Default `TRUE`. Whether to include vertex normals.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return List describing the mesh.
#' @export
#'
#' @examples
#' if(run_documentation()) {
#' #Generate a sphere in the Cornell box.
#' generate_cornell_mesh() |>
#'   add_shape(sphere_mesh(position = c(555/2, 555/2, 555/2), radius = 100)) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a shiny sphere in the Cornell box
#' generate_cornell_mesh() |>
#'   add_shape(sphere_mesh(position = c(555/2, 100, 555/2), radius = 100, 
#'                     material = material_list(diffuse = "gold",type="phong"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate an ellipsoid in the Cornell box
#' generate_cornell_mesh() |>
#'   add_shape(sphere_mesh(position = c(555/2, 210, 555/2), radius = 100, 
#'                         angle=c(0,30,0), scale = c(0.5,2,0.5),
#'                         material = material_list(diffuse = "dodgerblue",type="phong"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'}
sphere_mesh = function(position = c(0,0,0), 
                       scale = c(1,1,1), 
                       angle = c(0,0,0), 
                       pivot_point = c(0,0,0), 
                       order_rotation = c(1,2,3),
                       radius = 1, 
                       low_poly = FALSE,
                       normals = TRUE,
                       material = material_list()) {
  if(!low_poly) {
    obj = get("sphere", envir = ray_environment)
  } else {
    obj = get("low_poly_sphere", envir = ray_environment)
  }
  obj$vertices[[1]] = obj$vertices[[1]] * radius
  if(!normals) {
    obj$shapes[[1]]$has_vertex_normals = rep(FALSE,length(obj$shapes[[1]]$indices))
    obj$normals[[1]] = matrix(0,nrow=0,ncol=3)
  }
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
#' @return List describing the mesh.
#' @export
#'
#' @examples
#' if(run_documentation()) {
#' #Generate a cone
#' generate_cornell_mesh() |>
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2),
#'                       radius = 100)) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a blue cone with a wide base
#' generate_cornell_mesh() |>
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius=200,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a long, thin cone
#' generate_cornell_mesh() |>
#'   add_shape(cone_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 400, 555/2), radius=50,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'}
cone_mesh = function(start = c(0,0,0), end=c(0,1,0), 
                     radius = 0.5, direction = NA, from_center = FALSE,
                     material = material_list()) {
  obj = get("cone", envir = ray_environment)
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
    theta = atan2(as.numeric(-length_xy), as.numeric(end[2]-start[2]))/pi*180
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
#' @return List describing the mesh.
#' @export
#'
#' @examples
#' if(run_documentation()) {
#' #Generate an arrow
#' generate_cornell_mesh() |>
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius_tail=50,
#'                        radius_top = 100,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a blue arrow with a wide tail
#' generate_cornell_mesh() |>
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 300, 555/2), radius_tail=100,
#'                        radius_top = 150,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'   }
#' if(run_documentation()) {
#' #Generate a long, thin arrow and change the proportions
#' generate_cornell_mesh() |>
#'   add_shape(arrow_mesh(start = c(555/2, 20, 555/2), end = c(555/2, 400, 555/2), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Change the start and end points
#' generate_cornell_mesh() |>
#'   add_shape(arrow_mesh(start = c(500, 20, 555/2), end = c(50, 500, 555/2), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="dodgerblue"))) |>
#'   add_shape(arrow_mesh(start = c(500, 500, 500), end = c(50, 50, 50), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="red"))) |>
#'   add_shape(arrow_mesh(start = c(555/2, 50, 500), end = c(555/2, 50, 50), radius_top=30,
#'                        radius_tail = 10, tail_proportion = 0.8,
#'                       material = material_list(diffuse="green"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'}
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
    theta = atan2(as.numeric(-length_xy), as.numeric(end[2]-start[2]))/pi*180
  }
  fulllength = sqrt(sum((end-start)^2))
  angle = c(0, -phi, theta)
  
  obj = get("arrow", envir = ray_environment)
  obj = set_material(obj, material = material)
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(obj, material)
    
    new_radius = (2*radius_top) / (fulllength * (1-tail_proportion)) * (fulllength * (1-tail_proportion) + material[[1]]$toon_outline_width)
    
    obj2$vertices[[1]][c(1:32,66:97),c(1,3)] = obj$vertices[[1]][c(1:32,66:97),c(1,3)] * radius_tail/0.25 * (radius_tail + material[[1]]$toon_outline_width/2)/radius_tail
    obj2$vertices[[1]][34:65,c(1,3)] = obj$vertices[[1]][34:65,c(1,3)] * new_radius

    #Proportions
    obj2$vertices[[1]][33,2]            = (1 - 0.5) * fulllength + material[[1]]$toon_outline_width/2
    obj2$vertices[[1]][c(1:32,34:65),2] = (tail_proportion  - 0.5) * fulllength - material[[1]]$toon_outline_width/2
    obj2$vertices[[1]][66:97,2]         = (-0.5 * fulllength) - material[[1]]$toon_outline_width/2
    
    obj = add_shape(obj,obj2)
  }
  
  obj$vertices[[1]][c(1:32,66:97),c(1,3)] = obj$vertices[[1]][c(1:32,66:97),c(1,3)] * radius_tail/0.25
  obj$vertices[[1]][34:65,c(1,3)] = obj$vertices[[1]][34:65,c(1,3)] * radius_top/0.5
  
  #Proportions
  obj$vertices[[1]][33,2]            = (1 - 0.5) * fulllength
  obj$vertices[[1]][c(1:32,34:65),2] = (tail_proportion  - 0.5) * fulllength
  obj$vertices[[1]][66:97,2]         = (-0.5 * fulllength)

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
#' @return List describing the mesh.
#' @export
#' @examples
#' if(run_documentation()) {
#' #Generate a cylinder
#' generate_cornell_mesh() |>
#'   add_shape(cylinder_mesh(position=c(555/2,150,555/2),
#'                           radius = 50, length=300, material = material_list(diffuse="purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a wide, thin disk
#' generate_cornell_mesh() |>
#'   add_shape(cylinder_mesh(position=c(555/2,20,555/2),
#'                           radius = 200, length=5, material = material_list(diffuse="purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Generate a narrow cylinder
#' generate_cornell_mesh() |>
#'   add_shape(cylinder_mesh(position=c(555/2,555/2,555/2),angle=c(45,-45,0),
#'                           radius = 10, length=500, material = material_list(diffuse="purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0.5,0.5,-1)))
#'}
cylinder_mesh = function(position = c(0,0,0), radius = 0.5, length=1,
                         angle = c(0,0,0), 
                         pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                         material = material_list()) {
  obj = get("cylinder", envir = ray_environment)
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
#' @return List describing the mesh.
#' @export
#' @examples
#' if(run_documentation()) {
#' #Generate a segment in the cornell box. 
#' generate_cornell_mesh() |>
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(455, 455, 455), radius = 50)) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
#' if(run_documentation()) {
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
#' 
#' generate_cornell_mesh() |> 
#'   add_shape(scene_list) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
#' if(run_documentation()) {
#' #Draw the outline of a cube:
#' 
#' cube_outline = segment_mesh(start = c(100, 100, 100), end = c(100, 100, 455), radius = 10) |>
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(100, 455, 100), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 100, 100), end = c(455, 100, 100), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 100, 455), end = c(100, 455, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 100, 455), end = c(455, 100, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 455, 455), end = c(100, 455, 100), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 455, 455), end = c(455, 455, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(455, 455, 100), end = c(455, 100, 100), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(455, 455, 100), end = c(455, 455, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(455, 100, 100), end = c(455, 100, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(455, 100, 455), end = c(455, 455, 455), radius = 10)) |>
#'   add_shape(segment_mesh(start = c(100, 455, 100), end = c(455, 455, 100), radius = 10))
#' 
#' generate_cornell_mesh() |>
#'   add_shape(set_material(cube_outline,diffuse="dodgerblue",type="phong")) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'   }
#' if(run_documentation()) {
#' #Shrink and rotate the cube
#' generate_cornell_mesh() |>
#'   add_shape(
#'     scale_mesh(rotate_mesh(set_material(cube_outline,diffuse="dodgerblue",type="phong"),
#'                 angle=c(45,45,45), pivot_point=c(555/2,555/2,555/2)),0.5,
#'                 center=c(555/2,555/2,555/2))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
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
  phi =  atan2( as.numeric(end[1]-start[1]), as.numeric(end[3]-start[3]))/pi*180 + 90
  
  length_xy = sqrt((end[1]-start[1])^2 + (end[3]-start[3])^2)
  if(end[1] == start[1] && end[3] == start[3]) {
    if(start[2] - end[2] > 0) {
      theta = 180
    } else {
      theta = 0
    }
  } else {
    theta = atan2(as.numeric(-length_xy), as.numeric((end[2]-start[2])))/pi*180
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
#' @return List describing the mesh.
#' @export
#' @examples
#' if(run_documentation()) {
#' generate_cornell_mesh() |>
#'   add_shape(xy_rect_mesh(position = c(555/2, 100, 555/2), scale=200,
#'              material = material_list(diffuse = "purple"),angle=c(0,180,0))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Rotate the plane and scale 
#' generate_cornell_mesh() |>
#'   add_shape(xy_rect_mesh(position = c(555/2, 100, 555/2), scale=c(200,100,1), angle=c(0,180,0),
#'              material = material_list(diffuse = "purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
xy_rect_mesh = function(position = c(0,0,0), 
                        scale = c(1,1,1), 
                        angle = c(0,0,0), 
                        pivot_point = c(0,0,0), 
                        order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = get("xy_plane", envir = ray_environment)
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
#' @return List describing the mesh.
#' @export
#' @examples
#' if(run_documentation()) {
#' generate_cornell_mesh() |>
#'   add_shape(xz_rect_mesh(position = c(555/2, 100, 555/2), scale=200,
#'              material = material_list(diffuse = "purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#' }
#' if(run_documentation()) {
#' #Rotate the plane and scale 
#' generate_cornell_mesh() |>
#'   add_shape(xz_rect_mesh(position = c(555/2, 100, 555/2), scale=c(200,1,100), angle=c(0,30,0),
#'              material = material_list(diffuse = "purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
xz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                         angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = get("xz_plane", envir = ray_environment)
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
#' @return List describing the mesh.
#' @export
#' @examples
#' if(run_documentation()) {
#' generate_cornell_mesh() |>
#'   add_shape(yz_rect_mesh(position = c(555/2, 100, 555/2), scale=c(200,1,200), angle=c(0,0,0),
#'              material = material_list(diffuse = "purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
#' if(run_documentation()) {
#' #Rotate and scale
#' generate_cornell_mesh() |>
#'   add_shape(yz_rect_mesh(position = c(555/2, 100, 555/2), scale=c(300,1,200), angle=c(0,45,0),
#'              material = material_list(diffuse = "purple"))) |>
#'   rasterize_scene(light_info = directional_light(c(0,0.5,-1)))
#'}
yz_rect_mesh = function(position = c(0,0,0), scale = c(1,1,1), 
                        angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                        material = material_list()) {
  obj = get("yz_plane", envir = ray_environment)
  
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
  obj = set_material(obj, material = material)
  obj
}

#' Cornell Box 3D Model
#'
#' @param leftcolor Default `#1f7326` (green).
#' @param rightcolor Default `#a60d0d` (red).
#' @param roomcolor Default `#bababa` (light grey).
#' @param ceiling Default `TRUE`. Whether to render the ceiling.
#' @param light Default `TRUE`. Whether to render a point light near the ceiling.
#'@return List describing the mesh.
#'@export
#'@examples
#'if(run_documentation()) {
#' #Generate and render the default Cornell box and add an object.
#' generate_cornell_mesh() |> 
#'   rasterize_scene()
#' }
#' if(run_documentation()) {
#' #Add an object to the scene
#' generate_cornell_mesh() |> 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,555/2,555/2),scale=300,angle=c(0,180,0))) |> 
#'   rasterize_scene()
#' }
#' if(run_documentation()) {
#' #Turn off the ceiling so the default directional light reaches inside the box
#' generate_cornell_mesh(ceiling=FALSE) |> 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,555/2,555/2),scale=300,angle=c(0,180,0))) |> 
#'   rasterize_scene()
#' }
#' if(run_documentation()) {
#' #Adjust the light to the front
#' generate_cornell_mesh(ceiling=FALSE) |> 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,555/2,555/2),scale=300,angle=c(0,180,0))) |> 
#'   rasterize_scene(light_info = directional_light(direction=c(0,1,-1)))
#'   }
#' if(run_documentation()) {
#' #Change the color palette
#' generate_cornell_mesh(ceiling=FALSE,leftcolor="purple", rightcolor="yellow") |> 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,555/2,555/2),scale=300,angle=c(0,180,0))) |> 
#'   rasterize_scene(light_info = directional_light(direction=c(0,1,-1)))
#'}
generate_cornell_mesh = function(leftcolor = "#1f7326", 
                                 rightcolor = "#a60d0d", roomcolor = "#bababa", ceiling = TRUE,
                                 light = TRUE) {
  ambient_intensity = 0.25
  if(ceiling) {
    scene = set_material(cube_mesh(position=c(555+5,555/2,555/2),scale=c(10,555,555)),
                         diffuse=leftcolor,ambient = leftcolor, ambient_intensity=ambient_intensity) |>
      add_shape(set_material(cube_mesh(position=c(-5,555/2,555/2),angle=c(0,180,0),scale=c(10,555,555)),
                             diffuse=rightcolor,ambient = rightcolor, ambient_intensity=ambient_intensity)) |>
      add_shape(set_material(cube_mesh(position=c(555/2,555+5,555/2),scale=c(575,10,555)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) |>
      add_shape(set_material(cube_mesh(position=c(555/2,-5,555/2),scale=c(575,10,555)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) |> 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2-5,555+5),scale=c(575,565,10)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity))
  } else {
    scene = set_material(cube_mesh(position=c(555+5,555/2,555/2),scale=c(10,555,555)),
                         diffuse=leftcolor,ambient = leftcolor, ambient_intensity=ambient_intensity) |>
      add_shape(set_material(cube_mesh(position=c(-5,555/2,555/2),angle=c(0,180,0),scale=c(10,555,555)),
                             diffuse=rightcolor,ambient = rightcolor, ambient_intensity=ambient_intensity)) |>
      add_shape(set_material(cube_mesh(position=c(555/2,-5,555/2),scale=c(575,10,555)),
                             diffuse=roomcolor,ambient = roomcolor, ambient_intensity=ambient_intensity)) |> 
      add_shape(set_material(cube_mesh(position=c(555/2,555/2-5,555+5),scale=c(575,565,10)),
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
#' @param center Default `FALSE`. Whether to center the mesh.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#' @param material Default `NULL`, read from the MTL file. If not `NULL`, this accepts the output
#' from the `material_list()` function to specify the material.
#'@return List describing the mesh.
#'@export
#'@examples
#'if(run_documentation()) {
#' #Read in the provided 3D R mesh
#' generate_cornell_mesh(ceiling=FALSE) |> 
#'   add_shape(obj_mesh(r_obj(),position=c(555/2,555/2,555/2),scale=400,angle=c(0,180,0))) |> 
#'   rasterize_scene(light_info = directional_light(direction=c(0.2,0.5,-1)))
#'}
obj_mesh = function(filename, position = c(0,0,0), scale = c(1,1,1), 
                    angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), materialspath = NULL,
                    center = FALSE, material = NULL) {
  if(!file.exists(filename) || dir.exists(filename)) {
    stop(sprintf("OBJ `%s` not found or not an OBJ file", filename ))
  }
  obj_loaded = read_obj(filename, materialspath)

  if(any(scale != 1)) {
    obj_loaded = scale_mesh(obj_loaded, scale=scale)
  }
  if(length(obj_loaded$materials[[1]]) == 0 && is.null(material)) {
    material = material_list()
  }
  if(!is.null(material)) {
    obj_loaded = set_material(obj_loaded,material = material)
    if(material$type == "toon" || material$type == "toon_phong") {
      obj2 = generate_toon_outline(obj_loaded, material)
      obj_loaded = add_shape(obj_loaded,obj2)
    }
  }
  if(center) {
    obj_loaded = center_mesh(obj_loaded)
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
#'@return List describing the mesh.
#'@export
#'@examples
#'if(run_documentation()) {
#'#Plot a group of tori in the cornell box
#'generate_cornell_mesh(ceiling = FALSE) |> 
#'  add_shape(torus_mesh(position=c(555/2,555/3,555/2), angle=c(20,0,45),
#'                       radius=120, ring_radius = 40,
#'                       material = material_list(diffuse="dodgerblue4",type="phong",
#'                                                ambient="dodgerblue4",ambient_intensity=0.2))) |>
#'  add_shape(torus_mesh(position=c(400,400,555/2), angle=c(20,200,45),radius=80, ring_radius = 30,
#'                       material=material_list(diffuse="orange",type="phong",
#'                                              ambient="orange",ambient_intensity=0.2))) |>
#'  add_shape(torus_mesh(position=c(150,450,555/2), angle=c(60,180,0),radius=40, ring_radius = 20,
#'                       material=material_list(diffuse="red",type="phong"))) |>
#'  rasterize_scene(light_info = directional_light(c(0,1,-2)))
#'}
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
                      ring_radius = ring_radius + material[[1]]$toon_outline_width/2 , sides = sides, rings=rings,
                      material = material_list(diffuse=material[[1]]$toon_outline_color, 
                                               type="color",culling="front"))
    obj = add_shape(obj,obj2)
  }
  if(any(angle != 0)) {
    obj = rotate_mesh(obj, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  obj = translate_mesh(obj,position)
  obj
}

#' Mesh3d 3D Model
#'
#' @param mesh Mesh3d object.
#' @param center Default `FALSE`. Whether to center the mesh.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param materialspath Default `NULL`. Path to the MTL file, if different from the OBJ file.
#' @param material Default `NULL`, read from the MTL file. If not `NULL`, this accepts the output
#' from the `material_list()` function to specify the material.
#'@return List describing the mesh.
#'@export
#'@examples
#' if(run_documentation()) {
#'   # Read in a mesh3d object and rasterize it
#'   library(Rvcg)
#'   data(humface)
#'   
#'   mesh3d_mesh(humface,position = c(0,-0.3,0),scale = 1/70,
#'               material=material_list(diffuse="dodgerblue4", type="phong", shininess=20,
#'               ambient = "dodgerblue4", ambient_intensity=0.3)) |>
#'     rasterize_scene(lookat = c(0,0.5,1), light_info = directional_light(c(1,0.5,1)))
#'  }
#'  
#'  if(run_documentation()) {
#'   # Subdivide the mesh for a smoother appearance
#'   mesh3d_mesh(humface,position = c(0,-0.3,0),scale = 1/70,
#'               material=material_list(diffuse="dodgerblue4", type="phong", shininess=20,
#'               ambient = "dodgerblue4", ambient_intensity=0.3)) |>
#'     subdivide_mesh() |> 
#'     rasterize_scene(lookat = c(0,0.5,1), light_info = directional_light(c(1,0.5,1)))
#'  }
mesh3d_mesh = function(mesh, center = FALSE, position = c(0,0,0), scale = c(1,1,1), 
                       angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), materialspath = NULL,
                       material = material_list()) {
  mat_vals = mesh$material
  if(!is.null(mesh$texcoords)) {
    texcoords = t(mesh$texcoords)
    tex_indices = t(mesh$it)-1
  } else {
    texcoords = NULL
    tex_indices = NULL
  }
  if(!is.null(mat_vals)) {
    if(!is.null(mat_vals$color)) {
      diffuse_val = mat_vals$color
    } else {
      diffuse_val = material[[1]]$diffuse
    }
    if(!is.null(mat_vals$alpha)) {
      dissolve_val = mat_vals$alpha
    } else {
      dissolve_val = material[[1]]$dissolve
    }
    if(!is.null(mat_vals$ambient)) {
      ambient_val = mat_vals$ambient
    } else {
      ambient_val = material[[1]]$ambient
    }
    if(!is.null(mat_vals$shininess)) {
      exponent_val = mat_vals$shininess
    } else {
      exponent_val = material[[1]]$shininess
    }
    mesh = construct_mesh(vertices = t(mesh$vb)[,1:3], 
                          indices = t(mesh$it)-1,
                          texcoords = texcoords,
                          tex_indices = tex_indices,
                          material = material_list(
                            diffuse = diffuse_val,
                            dissolve = dissolve_val,
                            ambient =  ambient_val,
                            shininess = exponent_val))
  } else {
    mesh = construct_mesh(vertices = t(mesh$vb)[,1:3], 
                          indices = t(mesh$it)-1,
                          tex_indices = tex_indices,
                          texcoords = texcoords,
                          material = material)
  }

  if(any(scale != 1)) {
    mesh = scale_mesh(mesh, scale=scale)
  }
  if(material$type == "toon" || material$type == "toon_phong") {
    obj2 = generate_toon_outline(mesh, material)
    mesh = add_shape(mesh,obj2)
  }
  if(center) {
    obj_loaded = center_mesh(obj_loaded)
  }
  if(any(angle != 0)) {
    mesh = rotate_mesh(mesh, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  mesh = translate_mesh(mesh,position)
  mesh
}

#' Text Object
#'
#' @param label Text string.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param text_height Default `1`. Height of the text.
#' @param font Default `"sans"`. A character string specifying the font family (e.g., `"Arial"`, `"Times"`, `"Helvetica"`).
#' @param font_color Default `"black"`. The font color.
#' @param font_lineheight Default `12`. The lineheight for strings with newlines.
#' @param font_size Default `100`. The size of the font. Note that this does not control the size of the text, just the resolution
#' as rendered in the texture.
#' @param background_color Default `"white"`. The background color.
#' @param background_alpha Default `0`. The background opacity. `1` is fully opaque.
#' @param orientation Default `xy`. Orientation of the plane. Other options are `yz` and `xz`.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @importFrom  grDevices col2rgb
#'
#' @return List describing the mesh.
#' @export
#'
#' @examples
#' if(run_documentation()) {
#' #Generate a label in the Cornell box.
#' generate_cornell_mesh() |> 
#'   add_shape(text3d_mesh(label="Cornell Box", position=c(555/2,555/2,555/2),angle=c(0,180,0),
#'   text_height=120)) |> 
#'   rasterize_scene(light_info = directional_light(c(0.1,0.4,-1)))
#' }
#' if(run_documentation()) {
#' #Change the orientation
#' generate_cornell_mesh() |> 
#'   add_shape(text3d_mesh(label="YZ Plane", position=c(540,555/2,555/2),text_height=180,
#'                     orientation = "yz",angle=c(0,180,0))) |> 
#'   add_shape(text3d_mesh(label="XY Plane", position=c(555/2,555/2,540),text_height=180,
#'                     orientation = "xy", angle=c(0,180,0))) |> 
#'   add_shape(text3d_mesh(label="XZ Plane", position=c(555/2,15,555/2),text_height=180,
#'                     orientation = "xz", angle=c(0,180,0))) |> 
#'   rasterize_scene(light_info = directional_light(c(0.1,0.4,-1)))
#'   }
#' if(run_documentation()) {
#' #Add an label in front of a sphere and change the font
#' generate_cornell_mesh() |> 
#'   add_shape(text3d_mesh(label="Cornell Box", position=c(555/2,555/2,555/2),text_height=180,
#'                         font = "Serif", font_color="orange",
#'                         angle=c(0,180,0))) |> 
#'   add_shape(text3d_mesh(label="Sphere", position=c(555/2,130,100),text_height=100,
#'                         font = "sans",
#'                         font_color="lightblue",angle=c(0,180,40))) |> 
#'   add_shape(sphere_mesh(radius=100,position=c(555/2,100,555/2),
#'                         material=material_list(diffuse="purple",type="phong"))) |>                  
#'   rasterize_scene(light_info = directional_light(c(0.1,0.4,-1)))
#'   }
#' if(run_documentation()) {
#' #A room full of b's
#' set.seed(1)
#' bee_scene = list()
#' for(i in 1:100) {
#' bee_scene = add_shape(bee_scene, text3d_mesh("B", position=c(20+runif(3)*525), 
#'                                              font_color="yellow", text_height = 100,
#'                                              angle=c(0,180,0)))
#' }
#' generate_cornell_mesh() |> 
#'   add_shape(bee_scene) |>                   
#'   rasterize_scene(light=directional_light(c(0,1,-1)))
#'}
#'
#' if(run_documentation()) {
#' #A room full of bees
#' bee_scene = list()
#' set.seed(1)
#' for(i in 1:100) {
#'   bee_scene = add_shape(bee_scene, text3d_mesh("🐝", position=c(20+runif(3)*525), 
#'                                                font_color="yellow", text_height = 100,
#'                                                angle=c(0,180,0)))
#' }
#' generate_cornell_mesh() |> 
#'   add_shape(bee_scene) |>                   
#'   rasterize_scene(light=directional_light(c(0,1,-1)))
#'}
text3d_mesh = function(label, position = c(0,0,0), text_height = 1, orientation = "xy",
                       font_color = "black", font_size = 100,font = "sans", font_lineheight = 12,
                       background_color = "white", background_alpha = 0,
                       angle = c(0, 0, 0), pivot_point = c(0,0,0), order_rotation = c(1, 2, 3), 
                       scale = c(1,1,1)) {
  labelfile = tempfile(fileext = ".png")
  text_image = rayimage::render_text_image(label, 
                                           font = font, 
                                           size = font_size,
                                           color = font_color, 
                                           just = "left",
                                           lineheight = font_lineheight,
                                           background_color = background_color,
                                           background_alpha = background_alpha,
                                           filename = labelfile)
  height_val_raw = nrow(text_image)
  width_val_raw = ncol(text_image)
  ratio = text_height/height_val_raw
  width_val = width_val_raw * ratio
  if(orientation == "xy" || orientation == "yx") {
    mesh = xy_rect_mesh(position = position, angle = angle,pivot_point = pivot_point,
                        order_rotation = order_rotation,
                        scale = c(width_val, text_height,1))
  } else if (orientation == "yz" || orientation == "zy") {
    mesh = yz_rect_mesh(position = position, angle = angle,pivot_point = pivot_point,
                        order_rotation = order_rotation,
                        scale = c(1, text_height, width_val))
  } else if (orientation == "xz" || orientation == "zx") {
    mesh = xz_rect_mesh(position = position, angle = angle,pivot_point = pivot_point,
                        order_rotation = order_rotation,
                        scale = c(width_val, 1, text_height))
  } else {
    stop("Orientation ", orientation, " not recognized")
  }
  mesh = set_material(mesh, material = material_list(texture_location = labelfile, type="color",
                                                     culling = "none"))
  return(mesh)
}

#' PLY Mesh 3D Model
#'
#' @param filename PLY filename.
#' @param center Default `FALSE`. Whether to center the mesh.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#'@return List describing the mesh.
#'@export
#'@examples
#'#See the documentation for `obj_mesh()`--no example PLY models are included with this package,
#'#but the process of loading a model is the same (but no materials are included in PLY files).
ply_mesh = function(filename, center = FALSE, position = c(0,0,0), scale = c(1,1,1), 
                    angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3), 
                    material = material_list()) {
  ply_loaded = read_ply(filename)
  if(any(scale != 1)) {
    ply_loaded = scale_mesh(ply_loaded, scale=scale)
  }
  ply_loaded = set_material(ply_loaded,material = material)
  if(material$type == "toon" || material$type == "toon_phong") {
    ply2 = generate_toon_outline(ply_loaded, material)
    ply_loaded = add_shape(ply_loaded,ply2)
  }
  if(center) {
    ply_loaded = center_mesh(ply_loaded)
  }
  if(any(angle != 0)) {
    ply_loaded = rotate_mesh(ply_loaded, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  
  ply_loaded = translate_mesh(ply_loaded,position)
  ply_loaded
}
