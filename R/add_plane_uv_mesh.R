#' @title Add Plane UV Mapping to Mesh
#'
#' @description Applies a planar UV mapping to a mesh based on a given direction and set of U/V vectors.
#' If `full_mesh_bbox` is true, the UV mapping is scaled based on the bounding box 
#' of the entire mesh. If false, each shape's bounding box is used. 
#' One of direction/u/v must be `NULL` and will be calculated from the others.
#'
#' @param mesh The mesh to which the UV mapping will be applied.
#' @param direction Default `c(0, 1, 0)`. A vector specifying the direction for 
#' UV mapping. If not specified and `u`/`v` are both specified, this will be ignored.
#' @param u Default `NULL`. A vector specifying the u direction. 
#' @param v Default `NULL`. A vector specifying the v direction.
#' @param override_existing Default `FALSE`. Specifies whether existing UV 
#' coordinates should be overridden.
#' @param full_mesh_bbox Default `TRUE`. Specifies whether the full mesh's 
#' bounding box is used for UV mapping.
#'
#' @return Modified mesh with added UV mapping.
#' @export
#' @examples
#' if(run_documentation()) {
#' #Let's construct a mesh from the volcano dataset
#' #Build the vertex matrix
#' vertex_list = list()
#' counter = 1
#' for(i in 1:nrow(volcano)) {
#'   for(j in 1:ncol(volcano)) {
#'     vertex_list[[counter]] = matrix(c(j,volcano[i,j]/3,i), ncol=3)
#'     counter = counter + 1
#'   }
#' }
#' vertices = do.call(rbind,vertex_list)
#' 
#' #Build the index matrix
#' index_list = list()
#' counter = 0
#' for(i in 1:(nrow(volcano)-1)) {
#'   for(j in 1:(ncol(volcano)-1)) {
#'     index_list[[counter+1]] = matrix(c(counter,counter+ncol(volcano),counter+1,
#'                                        counter+ncol(volcano),counter+ncol(volcano)+1,counter + 1),
#'                                      nrow=2, ncol=3, byrow=TRUE)
#'     counter = counter + 1
#'   }
#'   counter = counter + 1
#' }
#' indices = do.call("rbind",index_list)
#' 
#' #Create a checkerboard image
#' create_checkerboard_texture = function(filename, n = 16) {
#'   old_par = par(no.readonly = TRUE)
#'   on.exit(par(old_par))
#'   plot.new()
#'   par(mar = c(0, 0, 0, 0))
#'   checkerboard = matrix(c(1, 0), nrow = n+1, ncol = n)
#'   png(filename, width = 800, height = 800)
#'   image(1:(n+1), 1:n, checkerboard, col = c("dodgerblue", "red"),
#'         axes = FALSE, xlab = "", ylab = "")
#'   dev.off()
#' }
#' checkerboard_file = tempfile(fileext = ".png")
#' create_checkerboard_texture(checkerboard_file)
#' rayimage::plot_image(checkerboard_file)
#' }
#' 
#' if(run_documentation()) {
#' #Construct the mesh
#' volc_mesh = construct_mesh(vertices = vertices, indices = indices,
#'                            material = material_list(type="phong", diffuse="darkred",
#'                                                     ambient = "darkred", ambient_intensity=0.2))
#' 
#' 
#' #Set the direction so that the checkerboard will be mapped to the surface like a carpet
#' uv = add_plane_uv_mesh(volc_mesh, direction=c(0,200,0), u = c(1,0,0))
#' uv = set_material(uv, texture_location = checkerboard_file,
#'                   ambient = "white", ambient_intensity=0.1)
#' #Rasterize the scene
#' rasterize_scene(center_mesh(uv), lookfrom=c(200,200,200),fov=0,width=1200,height=1200,
#'                 light_info = directional_light(c(0,1,1)) |>
#'                   add_light(directional_light(c(1,1,-1))),ortho_dimensions=c(120,120))
#' }
#' 
#' if(run_documentation()) {
#' #Set the direction so that the checkerboard will be mapped directly at the camera
#' uv = add_plane_uv_mesh(volc_mesh, direction=c(200,200,200), v = c(-1,1,-1))
#' uv = set_material(uv, texture_location = checkerboard_file,
#'                   ambient = "white", ambient_intensity=0.1)
#' #Rasterize the scene
#' rasterize_scene(center_mesh(uv), lookfrom=c(200,200,200),fov=0,width=1200,height=1200,
#'                 light_info = directional_light(c(0,1,1)) |>
#'                 add_light(directional_light(c(1,1,-1))), ortho_dimensions=c(120,120))
#'}
add_plane_uv_mesh = function(mesh, direction = c(0,1,0), u = NULL, v = NULL, 
                             override_existing = FALSE, full_mesh_bbox = TRUE) {
  duv = c(FALSE, FALSE, FALSE)
  if((is.null(direction) || missing(direction)) && !is.null(u) && !is.null(v)) {
    duv[1] = TRUE
  } 
  if(!is.null(direction) && is.null(u) && !is.null(v)) {
    duv[2] = TRUE
  }
  if(!is.null(direction) && !is.null(u) && is.null(v)) {
    duv[3] = TRUE
  }
  if(sum(duv) != 1) {
    stop("Exactly one of `direction`/`u`/`v` must be NULL")
  }
  if(duv[1]) {
    u = u / sqrt(sum(u^2))
    v = v / sqrt(sum(v^2))
    direction = cross_prod(u,v)
  } else if (duv[2]) {
    direction = direction / sqrt(sum(direction^2))
    v = v / sqrt(sum(v^2))
    u = cross_prod(direction,v)
  } else {
    direction = direction / sqrt(sum(direction^2))
    u = u / sqrt(sum(u^2))
    v = cross_prod(u,direction)
  }
  
  rot_mat = matrix(c(u,direction,v),nrow=3,ncol=3, byrow=TRUE)
  
  if(full_mesh_bbox) {
    all_verts = do.call("rbind", mesh$vertices)
    all_verts = t(apply(all_verts,1,\(x) rot_mat %*% x))
    xrange = range(all_verts[,1])
    zrange = range(all_verts[,3])
    stopifnot(xrange[1] != xrange[2] && zrange[1] != zrange[2])
  }
  # Iterate over shapes
  for (i in seq_along(mesh$shapes)) {
    shape = mesh$shapes[[i]]
    
    # Check if UV coords exist and whether they should be overridden
    if (!override_existing && any(shape$has_vertex_tex)) next
    
    vertices_bbox = mesh$vertices[[i]]
    vertices_bbox = t(apply(vertices_bbox,1,\(x) rot_mat %*% x))
    
    if(!full_mesh_bbox) {
      xrange = range(vertices_bbox[,1])
      zrange = range(vertices_bbox[,3])
      stopifnot(xrange[1] != xrange[2] && zrange[1] != zrange[2])
    }

    vertices_bbox[,1] = (vertices_bbox[,1] - xrange[1])/(xrange[2]- xrange[1])
    vertices_bbox[,3] = (vertices_bbox[,3] - zrange[1])/(zrange[2]- zrange[1])
    
    # Initialize a matrix to store UV coordinates
    uv_coords = vertices_bbox[,c(3,1)]

    # Assign the UV coords to the shape
    mesh$texcoords[i] = ray_vertex_data(uv_coords)
    mesh$shapes[[i]]$tex_indices = mesh$shapes[[i]]$indices
    mesh$shapes[[i]]$has_vertex_tex = rep(TRUE, nrow(mesh$shapes[[i]]$indices))
  }
  
  return(mesh)
}

#' @title Add Sphere UV Mapping to Mesh
#'
#' @description Applies a planar UV mapping to a mesh based on a spherical
#' direction from the origin.
#' 
#' @param mesh The mesh to which the UV mapping will be applied.
#' @param origin Default `c(0, 0, 0)`. A vector specifying the origin to 
#' apply spherical UV coordinates.
#' @param override_existing Default `FALSE`. Specifies whether existing UV 
#' coordinates should be overridden.
#' @param full_mesh_bbox Default `TRUE`. Specifies whether the full mesh's 
#' bounding box is used for UV mapping.
#'
#' @return Modified mesh with added UV mapping.
#' @export
#' @examples
#' if(run_documentation()) {
#' #Let's construct a mesh from the volcano dataset
#' 
#'}
add_sphere_uv_mesh = function(mesh, 
                              origin = c(0, 0, 0), 
                              override_existing = FALSE) {
  stopifnot(length(origin) == 3 && is.numeric(origin))
  # Iterate over shapes
  for (i in seq_along(mesh$shapes)) {
    shape = mesh$shapes[[i]]
    
    # Check if UV coords exist and whether they should be overridden
    if (!override_existing && any(shape$has_vertex_tex)) next
    
    vertices = mesh$vertices[[i]]
    
    vertices_centered = vertices - matrix(origin,nrow=nrow(vertices), ncol=3,byrow=TRUE)
    
    # Initialize a matrix to store UV coordinates
    uv_coords = matrix(0, nrow = nrow(vertices_centered), ncol = 2)
    
    # Calculate spherical coordinates
    for (j in 1:nrow(vertices_centered)) {
      x = vertices_centered[j, 1]
      y = vertices_centered[j, 2]
      z = vertices_centered[j, 3]
      
      r = sqrt(x^2 + y^2 + z^2)
      theta = acos(y / r)
      phi = -atan2(z, x)
      # Normalize theta and phi to [0, 1] range
      u = (phi + pi) / (2 * pi)
      v = 1-theta / pi
      
      uv_coords[j, ] = c(u, v)
    }
    
    # Assign the UV coords to the shape
    mesh$texcoords[i] = ray_vertex_data(uv_coords)
    mesh$shapes[[i]]$tex_indices = mesh$shapes[[i]]$indices
    mesh$shapes[[i]]$has_vertex_tex = rep(TRUE, nrow(mesh$shapes[[i]]$indices))
  }
  
  return(mesh)
}

#' @title Cross Product
#'
#' @description Computes the cross product of two 3-dimensional vectors.
#'
#' @param x A numeric vector representing the first 3D vector.
#' @param y A numeric vector representing the second 3D vector.
#'
#' @return A numeric vector representing the cross product of `x` and `y`.
#'
#' @keywords internal
cross_prod = function(x,y) {
  return(c(x[2]*y[3]-x[3]*y[2],
           -(x[1]*y[3]-x[3]*y[1]),
           x[1]*y[2]-x[2]*y[1]))
}

#' @title Generate Rotation Matrix from Direction
#'
#' @description Internal function to generate a rotation matrix that aligns the 
#' Y-axis with a given direction vector. It uses the Rodrigues' rotation formula.
#'
#' @param direction Default `c(0, 1, 0)`. A 3D vector representing the direction.
#' The function normalizes this vector.
#' @param up Default `c(0, 1, 0)`. A 3D vector representing the up direction.
#'
#' @return A 3x3 rotation matrix.
#'
#' @keywords internal
generate_rotation_matrix_from_direction = function(direction = c(0, 1, 0), up = c(0, 1, 0)) {
  # Ensure the vectors are unit vectors
  direction = direction / sqrt(sum(direction^2))
  up = up / sqrt(sum(direction^2))
  
  # The rotation axis is the cross product of the direction vector and the y-axis
  rotation_axis = cross_prod(matrix(direction, ncol = 3), matrix(up, ncol = 3))
  
  # The sine of the angle is the magnitude of the rotation axis
  sin_angle = sqrt(sum(rotation_axis^2))
  
  # The cosine of the angle is the dot product of the direction and the y-axis
  cos_angle = sum(direction * up)
  
  # Normalizing the rotation axis
  rotation_axis = rotation_axis / sin_angle
  
  # The rotation axis components
  ux = rotation_axis[1]
  uy = rotation_axis[2]
  uz = rotation_axis[3]
  
  # Constructing the rotation matrix using Rodrigues' rotation formula
  R = matrix(c(
    cos_angle + ux^2 * (1 - cos_angle), ux * uy * (1 - cos_angle) - uz * sin_angle, ux * uz * (1 - cos_angle) + uy * sin_angle,
    uy * ux * (1 - cos_angle) + uz * sin_angle, cos_angle + uy^2 * (1 - cos_angle), uy * uz * (1 - cos_angle) - ux * sin_angle,
    uz * ux * (1 - cos_angle) - uy * sin_angle, uz * uy * (1 - cos_angle) + ux * sin_angle, cos_angle + uz^2 * (1 - cos_angle)
  ), nrow = 3, byrow = TRUE)
  
  return(t(R))
}
