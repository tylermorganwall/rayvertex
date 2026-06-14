#' Construct Displacement Sphere
#'
#' @param displacement_texture Image or matrix/array that will be used to displace the sphere.
#' @param displacement_scale Default `1`. Scale of the displacement.
#' @param use_cube Default `FALSE`. Whether to use a subdivided cube instead of a UV sphere. Use this
#' if you want to visualize areas near the poles.
#' @param cube_subdivision_levels Default `NA`. Number of times to subdivide the cube before
#' projecting it to a sphere. When `NA`, this is calculated from the displacement texture
#' resolution as `max(1, ceiling(log((width * height) / 12) / log(4)))`, where
#' `width * height` is the number of pixels in the texture, `12` is the number of starting
#' cube triangles, and each subdivision level increases the triangle count by about `4`.
#' @param displace Default `TRUE`. Whether to displace the sphere, or just generate the initial mesh
#' for later displacement.
#' @param verbose Default `TRUE`. Whether to print displacement texture information.
#' @param position Default `c(0,0,0)`. Position of the mesh.
#' @param scale Default `c(1,1,1)`. Scale of the mesh. Can also be a single numeric value scaling all axes uniformly.
#' @param angle Default `c(0,0,0)`. Angle to rotate the mesh.
#' @param pivot_point Default `c(0,0,0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1,2,3)`. Order to rotate the axes.
#' @param material Default [material_list()] (default values). Specify the material of the object.
#'
#' @return raymesh object
#' @export
#' @examplesIf interactive() || isTRUE(as.logical(Sys.getenv("IN_PKGDOWN")))
#' texture_dim = 800
#' u = seq(0, 2 * pi, length.out = texture_dim)
#' v = seq(0, 2 * pi, length.out = texture_dim)
#' knit_texture = outer(u, v, function(x, y) {
#'   sin(10 * x + 0.75 * sin(10 * y))^2 + 0.35 * cos(12 * y)^2
#' })
#' knit_texture = (knit_texture - min(knit_texture)) / diff(range(knit_texture))
#' knit_texture = array(
#'   rep(knit_texture, 3),
#'   dim = c(texture_dim, texture_dim, 3)
#' )
#' rayimage::plot_image(knit_texture)
#' light_info = directional_light(c(1,1,1), color="dodgerblue",intensity=0.8) |>
#'                 add_light(directional_light(c(-1,-1,0.1), color="red",intensity=0.8)) |>
#'                 add_light(directional_light(c(0.5,1,0.5),intensity=0.8))
#' displacement_sphere(knit_texture,
#' displacement_scale = 0.08, verbose = TRUE) |>
#'   rasterize_scene(light_info = light_info, fov=15)
#'
#' #The default sphere has issues near the poles
#' displacement_sphere(knit_texture, displacement_scale = 0.08) |>
#'   rasterize_scene(light_info = light_info, fov=10, lookfrom=c(0,10,10))
#'
#' # A cube will render more nicely near the poles
#' displacement_sphere(knit_texture, use_cube = TRUE, displacement_scale = 0.08) |>
#'   rasterize_scene(light_info = light_info, fov=10, lookfrom=c(0,10,10))
displacement_sphere = function(
  displacement_texture,
  displacement_scale = 1,
  use_cube = FALSE,
  cube_subdivision_levels = NA,
  displace = TRUE,
  verbose = TRUE,
  position = c(0, 0, 0),
  scale = c(1, 1, 1),
  angle = c(0, 0, 0),
  pivot_point = c(0, 0, 0),
  order_rotation = c(1, 2, 3),
  material = material_list()
) {
  displacement_texture = rayimage::ray_read_image(
    displacement_texture,
    convert_to_array = FALSE
  )

  if (use_cube) {
    map_cube_to_sphere = function(mesh) {
      project_vertex_to_sphere = function(x) {
        x / sqrt(sum(x * x))
      }

      mesh$vertices[[1]] = t(apply(
        mesh$vertices[[1]],
        1,
        project_vertex_to_sphere
      ))
      add_sphere_uv_mesh(mesh, override_existing = TRUE) |>
        smooth_normals_mesh()
    }
    if (is.na(cube_subdivision_levels)) {
      initial_triangles = 12
      subdivision_factor = 4
      pixels = prod(dim(displacement_texture)[1:2])
      cube_subdivision_levels = max(
        1,
        ceiling(log(pixels / initial_triangles) / log(subdivision_factor))
      )
    }

    raymesh_new = cube_mesh() |>
      subdivide_mesh(subdivision_levels = cube_subdivision_levels) |>
      map_cube_to_sphere()
  } else {
    map_grid_to_sphere <- function(x, y) {
      # Convert x and y to degrees
      longitude = x * 180 # Scale -1 to 1 to -180 to 180
      latitude = y * 90 # Scale -1 to 1 to -90 to 90

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
    raymesh_surface = generate_surface(matrix(
      0,
      ncol = nrow(displacement_texture),
      nrow = ncol(displacement_texture)
    ))
    range_x = range(raymesh_surface$verts[, 1])
    raymesh_surface$verts[, 1] = raymesh_surface$verts[, 1] / range_x[2]
    range_z = range(raymesh_surface$verts[, 3])
    raymesh_surface$verts[, 3] = raymesh_surface$verts[, 3] / range_z[2]
    raymesh_surface$verts[, 2] = 0

    spherized_mesh_verts = map_grid_to_sphere(
      raymesh_surface$verts[, 1],
      raymesh_surface$verts[, 3]
    )

    new_texcoords = matrix(
      c(c(-raymesh_surface$verts[, 1], raymesh_surface$verts[, 3]) / 2 + 0.5),
      ncol = 2
    )

    raymesh_new = construct_mesh(
      vertices = spherized_mesh_verts,
      indices = t(raymesh_surface$inds) - 1,
      tex_indices = t(raymesh_surface$inds) - 1,
      norm_indices = t(raymesh_surface$inds) - 1,
      texcoords = new_texcoords,
      normals = t(apply(spherized_mesh_verts, 1, \(x) x / sqrt(sum(x * x)))),
      material = material
    )
  }
  if (any(scale != 1)) {
    raymesh_new = scale_mesh(raymesh_new, scale = scale)
  }
  if (any(angle != 0)) {
    raymesh_new = rotate_mesh(
      raymesh_new,
      angle = angle,
      pivot_point = pivot_point,
      order_rotation = order_rotation
    )
  }
  raymesh_new = translate_mesh(raymesh_new, position)
  if (displace) {
    return(displace_mesh(
      raymesh_new,
      displacement_texture,
      displacement_scale,
      verbose = verbose
    ))
  } else {
    return(raymesh_new)
  }
}
