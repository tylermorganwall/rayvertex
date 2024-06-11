#' Construct Displacement Sphere
#' 
#' @param displacement_texture Image or matrix/array that will be used to displace the sphere.
#' @param displacement_scale Default `1`. Scale of the displacement.
#' @param displace Default `TRUE`. Whether to displace the sphere, or just generate the initial mesh 
#' for later displacement.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#' @return raymesh object
#' @export
#' @examples
#' if(run_documentation()) {

#'}
displacement_sphere = function(displacement_texture, displacement_scale = 1,
                               displace = TRUE, verbose = TRUE,
                               position = c(0,0,0), scale = c(1,1,1), 
                               angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3),
                               material = material_list()) {
  map_grid_to_sphere <- function(x, y) {
    # Convert x and y to degrees
    longitude = x * 180  # Scale -1 to 1 to -180 to 180
    latitude = y * 90    # Scale -1 to 1 to -90 to 90
    
    # Convert degrees to radians
    longitude_rad = longitude * pi / 180 
    latitude_rad = latitude * pi / 180
    
    # Convert spherical to Cartesian coordinates
    X = cos(latitude_rad) * cos(longitude_rad)
    Z = cos(latitude_rad) * sin(longitude_rad)
    Y = sin(latitude_rad)
    
    # Return a matrix of sphere coordinates
    return(cbind(X, Y, Z))
  }
  displacement_texture = rayimage::ray_read_image(displacement_texture, convert_to_array = FALSE)
  raymesh_surface = generate_surface(matrix(0,ncol=nrow(displacement_texture), nrow = ncol(displacement_texture)))
  range_x = range(raymesh_surface$verts[,1])
  raymesh_surface$verts[,1] = raymesh_surface$verts[,1]/range_x[2]
  range_z = range(raymesh_surface$verts[,3])
  raymesh_surface$verts[,3] = raymesh_surface$verts[,3]/range_z[2]
  raymesh_surface$verts[,2] = 0
  
  spherized_mesh_verts = map_grid_to_sphere(raymesh_surface$verts[,1], 
                                            raymesh_surface$verts[,3])

  new_texcoords = matrix(c(c(-raymesh_surface$verts[,1],
                             raymesh_surface$verts[,3])/2+0.5), ncol=2)
  
  raymesh_new = construct_mesh(vertices = spherized_mesh_verts,
                               indices = t(raymesh_surface$inds)-1,
                               tex_indices = t(raymesh_surface$inds)-1,
                               norm_indices =  t(raymesh_surface$inds)-1,
                               texcoords = new_texcoords,
                               normals = t(apply(spherized_mesh_verts, 1, \(x) x/sqrt(sum(x*x)))),
                               material = material)
  
  if(any(scale != 1)) {
    raymesh_new = scale_mesh(raymesh_new, scale=scale)
  }
  if(any(angle != 0)) {
    raymesh_new = rotate_mesh(raymesh_new, angle=angle, pivot_point=pivot_point, order_rotation = order_rotation)
  }
  raymesh_new = translate_mesh(raymesh_new,position)
  if(displace) {
    return(displace_mesh(raymesh_new, displacement_texture, displacement_scale, verbose = verbose))
  } else {
    return(raymesh_new)
  }
}