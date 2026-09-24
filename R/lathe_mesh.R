#' Surface of Revolution Mesh
#'
#' Revolve a radius/height profile around the Y axis to create a triangle mesh.
#'
#' @param profile Numeric matrix or data frame with two columns: nonnegative
#' radius and Y coordinate. Rows trace the surface in order. Use a bottom-to-top
#' profile for outward-facing sides; for a hollow vessel, continue over the rim
#' and down the inside. Reversing the profile reverses the surface orientation.
#' @param segments Default `128`. Integer number of segments around the axis,
#' at least 3.
#' @param smooth Default `TRUE`. Whether to include smooth vertex normals.
#' Set to `FALSE` for flat triangle shading.
#' @param position Default `c(0, 0, 0)`. Position of the mesh.
#' @param scale Default `c(1, 1, 1)`. Scale of the mesh. A single number scales
#' all axes uniformly.
#' @param angle Default `c(0, 0, 0)`. Rotation angles in degrees.
#' @param pivot_point Default `c(0, 0, 0)`. Point around which to rotate the mesh.
#' @param order_rotation Default `c(1, 2, 3)`. Order to rotate the axes.
#' @param material Default `material_list()`. Material of the mesh.
#'
#' @details
#' Positive-radius endpoints leave open rings. To close an end, include a
#' zero-radius endpoint in the profile. Each axis endpoint uses a single vertex,
#' and the angular seam shares vertices, so a simple profile with both endpoints
#' on the axis produces a watertight mesh without degenerate cap triangles.
#' Axis points are only allowed at the endpoints. Consecutive duplicate points
#' and immediate reversals are rejected; other self-intersections are not checked.
#' The first and last profile rows are not automatically joined.
#'
#' Smooth normals average adjacent unit profile tangents. Axis endpoint normals
#' point along the axis. Texture coordinates use angle for U and normalized
#' distance along the profile for V. Separate texture indices allow U to wrap
#' from 1 to 0 without duplicating geometry at the seam.
#'
#' @return A `ray_mesh` object, usable with [rasterize_scene()],
#' [add_shape()], and [write_scene_to_obj()].
#' @seealso [draw_lathe_profile()] to draw a profile interactively.
#' @export
#' @examples
#' # A closed bottle, specified as (radius, height) pairs.
#' profile = rbind(c(0, 0), c(1, 0), c(1, 2), c(0.4, 2.5),
#'                 c(0.4, 3), c(0, 3))
#' plot(profile, type="l")
#' bottle = lathe_mesh(profile, material = material_list(diffuse = "steelblue"))
#' rasterize_scene(bottle, lookfrom = c(5, 5, 7), lookat = c(0, 1.5, 0),
#'                 fov = 30, light_info = directional_light(c(1, 2, 3)))
#'
#' # A hollow tumbler with a solid bottom and an open mouth.
#' t_profile = rbind(c(0, 0), c(1, 0), c(1.1, 3),
#'                           c(1, 3), c(0.9, 0.15), c(0, 0.15))
#' plot(t_profile, type="l")
#' tumbler = lathe_mesh(t_profile)
#' rasterize_scene(tumbler, lookfrom = c(5, 5, 7), lookat = c(0, 1.5, 0),
#'                 fov = 30, light_info = directional_light(c(1, 2, 3)),
#'                 shadow_map = FALSE)
lathe_mesh = function(
  profile,
  segments = 128,
  smooth = TRUE,
  position = c(0, 0, 0),
  scale = c(1, 1, 1),
  angle = c(0, 0, 0),
  pivot_point = c(0, 0, 0),
  order_rotation = c(1, 2, 3),
  material = material_list()
) {
  if (is.data.frame(profile)) {
    profile = as.matrix(profile)
  }
  if (
    !is.matrix(profile) ||
      !is.numeric(profile) ||
      ncol(profile) != 2L ||
      nrow(profile) < 2L ||
      any(!is.finite(profile))
  ) {
    stop(
      "`profile` must be a finite numeric two-column matrix with at least two rows."
    )
  }
  if (any(profile[, 1] < 0)) {
    stop("`profile` radii must be nonnegative.")
  }
  if (
    !is.numeric(segments) ||
      length(segments) != 1L ||
      !is.finite(segments) ||
      segments < 3 ||
      segments != floor(segments) ||
      segments > .Machine$integer.max
  ) {
    stop(
      "`segments` must be an integer of at least 3 within the R integer range."
    )
  }
  if (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth)) {
    stop("`smooth` must be TRUE or FALSE.")
  }
  segments = as.integer(segments)
  count = nrow(profile)
  axis = profile[, 1] == 0
  if (any(axis[-c(1L, count)]) || all(axis)) {
    stop(
      "Zero radii are only allowed at profile endpoints, with positive radii between them."
    )
  }
  tangent = profile[-1L, , drop = FALSE] - profile[-count, , drop = FALSE]
  lengths = sqrt(rowSums(tangent^2))
  if (
    any(!is.finite(lengths)) || any(lengths == 0) || !is.finite(sum(lengths))
  ) {
    stop(
      "Consecutive profile points must be distinct with finite, nonzero distances."
    )
  }
  tangent = tangent / lengths
  averaged = rbind(tangent[1L, ], tangent) +
    rbind(tangent, tangent[count - 1L, ])
  normal_lengths = sqrt(rowSums(averaged^2))
  if (any(normal_lengths < sqrt(.Machine$double.eps))) {
    stop("`profile` must not reverse direction at consecutive segments.")
  }
  averaged = averaged / normal_lengths

  theta = 2 * pi * (seq_len(segments) - 1L) / segments
  ring_sizes = ifelse(axis, 1L, segments)
  offsets = c(0, cumsum(ring_sizes))
  vertices = normals = vector("list", count)
  rings = vector("list", count)
  for (i in seq_len(count)) {
    angles = if (axis[i]) 0 else theta
    vertices[[i]] = cbind(
      profile[i, 1] * cos(angles),
      profile[i, 2],
      profile[i, 1] * sin(angles)
    )
    normals[[i]] = if (axis[i]) {
      matrix(c(0, -sign(averaged[i, 1]), 0), nrow = 1L)
    } else {
      cbind(
        averaged[i, 2] * cos(angles),
        -averaged[i, 1],
        averaged[i, 2] * sin(angles)
      )
    }
    rings[[i]] = offsets[i] + seq_len(ring_sizes[i]) - 1L
  }
  # UVs have an extra column at the seam, independent of geometric vertices.
  u = seq(0, 1, length.out = segments + 1)
  v = c(0, cumsum(lengths)) / sum(lengths)
  texcoords = cbind(rep(u, count), rep(v, each = segments + 1))
  faces = texture_faces = vector("list", count - 1L)
  j = seq_len(segments)
  k = j %% segments + 1L
  for (i in seq_len(count - 1L)) {
    lower = rings[[i]]
    upper = rings[[i + 1L]]
    lower_uv = (i - 1) * (segments + 1) + j - 1L
    upper_uv = i * (segments + 1) + j - 1L
    if (axis[i]) {
      faces[[i]] = cbind(lower, upper[j], upper[k])
      texture_faces[[i]] = cbind(lower_uv, upper_uv, upper_uv + 1L)
    } else if (axis[i + 1L]) {
      faces[[i]] = cbind(lower[j], upper, lower[k])
      texture_faces[[i]] = cbind(lower_uv, upper_uv, lower_uv + 1L)
    } else {
      faces[[i]] = rbind(
        cbind(lower[j], upper[j], upper[k]),
        cbind(lower[j], upper[k], lower[k])
      )
      texture_faces[[i]] = rbind(
        cbind(lower_uv, upper_uv, upper_uv + 1L),
        cbind(lower_uv, upper_uv + 1L, lower_uv + 1L)
      )
    }
  }
  indices = do.call(rbind, faces)
  mesh = construct_mesh(
    vertices = do.call(rbind, vertices),
    indices = indices,
    normals = if (smooth) do.call(rbind, normals) else NULL,
    norm_indices = if (smooth) indices else NULL,
    texcoords = texcoords,
    tex_indices = do.call(rbind, texture_faces),
    material = material
  )
  if (any(scale != 1)) {
    mesh = scale_mesh(mesh, scale = scale)
  }
  if (any(angle != 0)) {
    mesh = rotate_mesh(
      mesh,
      angle = angle,
      pivot_point = pivot_point,
      order_rotation = order_rotation
    )
  }
  translate_mesh(mesh, position)
}
