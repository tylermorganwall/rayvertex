#'@title Displace a Mesh
#'
#' @param mesh The mesh.
#' @param displacement_texture Image or matrix/array that will be used to displace the mesh
#' @param displacement_scale Default `1`. Intensity of the displacement effect. 
#' Higher values result in greater displacement.
#' @param displacement_vector Default `FALSE`. Whether to use vector displacement. 
#' If `TRUE`, the displacement texture is interpreted as providing a 3D displacement vector. 
#' Otherwise, the texture is interpreted as providing a scalar displacement.
#' @param id Default `NA` (all shapes). The shape index to have new normals calculated.
#' @param verbose Default `TRUE`. Whether to print displacement texture information.
#'
#'@return raymesh object
#'@export
#'@examples
#'if(run_documentation()) {
#'  #Let's construct a mesh from the volcano dataset
#' }
displace_mesh = function(mesh, 
                         displacement_texture,
                         displacement_scale = 1,
                         displacement_vector = FALSE,
                         id = NA, verbose = TRUE) {
  stopifnot(inherits(mesh,"ray_mesh"))
  displacement_texture = rayimage::ray_read_image(displacement_texture)
  if(displacement_vector) {
    disp_x = displacement_texture[,,1]
    disp_y = displacement_texture[,,2]
    disp_z = displacement_texture[,,3]
  } else {
    disp_x = t(displacement_texture[,,1])
    disp_y = matrix(0,0,0)
    disp_z = matrix(0,0,0)
  }
  if(verbose) {
    message(sprintf("Displacing mesh with %ix%i texture", dim(displacement_texture)[1],dim(displacement_texture)[2]))
  }
  if(is.na(id)) {
    for(i in seq_len(length(mesh$shapes))) {
      tangents = matrix(0,0,0)
      tangent_right_handed = vector("logical",0)
      if(displacement_vector) {
        tangent_info = CalculateTangents(mesh, i-1)
        tangents = tangent_info$tangents
        tangent_right_handed = tangent_info$tangent_right_handed
      }
      new_vertices = DisplaceMesh(mesh, 
                                  disp_x,
                                  disp_y,
                                  disp_z,
                                  tangents,
                                  tangent_right_handed,
                                  displacement_scale, 
                                  displacement_vector,
                                  i-1)
      mesh$vertices[i] = ray_vertex_data(new_vertices)
    }
    mesh = smooth_normals_mesh(mesh)
  } else {
    stopifnot(id <= length(mesh$shapes))
    tangents = matrix(0,0,0)
    tangent_right_handed = vector("logical",0)
    if(displacement_vector) {
      tangent_info = CalculateTangents(mesh, id-1)
      tangents = tangent_info$tangents
      tangent_right_handed = tangent_info$tangent_right_handed
    }
    new_vertices = DisplaceMesh(mesh, 
                                disp_x,
                                disp_y,
                                disp_z,
                                tangents,
                                tangent_right_handed,
                                displacement_scale, 
                                displacement_vector,
                                id-1)
    mesh$vertices[id] = ray_vertex_data(new_vertices)
    mesh = smooth_normals_mesh(mesh, id)
  }
  return(mesh)
}
