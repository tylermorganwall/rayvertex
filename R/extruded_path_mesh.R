#' Extrude a Profile Along a Path
#'
#' Construct an indexed sweep with independent geometry, normal, and texture
#' indices. Geometry construction is independent of any renderer.
#'
#' @param points Matrix or data frame with three columns, or a list of numeric
#' three-vectors, describing the path. With precomputed controls, a list of
#' four-by-three cubic Bezier control-point matrices.
#' @param polygon Default `NA`. Cross-section accepted by `xy.coords()`;
#' defaults to a 30-sided circle with diameter one. Must be a simple polygon
#' without holes. A repeated closing vertex is removed.
#' @param polygon_end Default `NA`. Optional final profile with the same number
#' of vertices. Corresponding rows are interpolated along the full path.
#' @param breaks Default `NA`. Samples along the full path, including endpoints;
#' defaults to 20 times the number of Bezier segments. Exact trim endpoints and
#' polyline corners are inserted as needed.
#' @param closed Default `FALSE`. Close the path and align its end frames.
#' @param closed_smooth Default `TRUE`. Use C2-continuous periodic interpolation
#' for a closed smooth path. Otherwise its closing segment has C1 continuity.
#' Ignored for straight paths and precomputed controls.
#' @param polygon_add_points Default `0`. Number of points to insert per
#' profile edge before sweeping.
#' @param twists Default `0`. Number of full profile rotations over the path.
#' @param texture_repeats Default `1`. Texture repetitions along the full path.
#' @param straight Default `FALSE`. Connect points with straight segments.
#' @param precomputed_control_points Default `FALSE`. Interpret `points` as
#' cubic Bezier controls instead of interpolation points.
#' @param width Default `1`. Nonnegative profile scale, or numeric width values
#' spaced evenly along the path. An `xy.coords()` representation supplies
#' explicit positions from zero to one and widths. Zero-width rings form triangle fans.
#' @param width_end Default `NA`. Final width when `width` is a scalar;
#' otherwise ignored. Defaults to the initial width.
#' @param width_ease Default `"spline"`. Shape-preserving cubic interpolation
#' between widths. Other choices are `linear`, `quad`, `cubic`, and `exp`.
#' @param smooth_normals Default `FALSE`. Include surface normals accounting
#' for taper, twist, and changing profiles. Cap normals remain hard.
#' @param u_min Default `0`. Start of the retained path interval. Closed paths
#' can wrap across their seam.
#' @param u_max Default `1`. End of the retained path interval. A full closed
#' interval has no caps. Equal endpoints produce an empty mesh.
#' @param linear_step Default `FALSE`. Interpret path coordinates as normalized
#' arc length using adaptive sampling instead of Bezier parameter distance.
#' @param end_caps Default `c(TRUE, TRUE)`. Include the starting and ending caps.
#' @param initial_normal Default `NULL`. Initial direction of the profile's
#' positive X axis, projected perpendicular to the path tangent. Must not be
#' parallel to that tangent. By default curvature or a stable axis is used.
#' @param smooth_angle Default `180`. Maximum profile turning angle in degrees
#' to smooth. Smaller values preserve corners when `smooth_normals = TRUE`.
#' @param arc_tolerance Default `1e-5`. Relative adaptive sampling tolerance,
#' measured against the total control-polygon length.
#' @param position Default `c(0, 0, 0)`. Translation of the completed mesh.
#' @param scale Default `c(1, 1, 1)`. Scale of the mesh, uniformly or per axis.
#' @param angle Default `c(0, 0, 0)`. Rotation angles in degrees.
#' @param pivot_point Default `c(0, 0, 0)`. Point around which to rotate.
#' @param order_rotation Default `c(1, 2, 3)`. Order to rotate the axes.
#' @param material Default `material_list()`. Surface material.
#' @param material_caps Default `NULL`. Cap material; defaults to `material`.
#' @param separate_caps Default `FALSE`. Return surfaces and caps separately
#' for independent editing or material assignment by another renderer.
#'
#' @details
#' Closed sweeps correct accumulated frame rotation. Full loops and intervals
#' crossing the seam require compatible endpoint profiles, widths, and twists.
#' For example, a square profile permits quarter-turn seam rotations. Invalid
#' seams raise an error. Profile winding is normalized while retaining the first
#' vertex. Texture seams and shading creases use separate attribute indices.
#'
#' @return A `ray_mesh`, or `NULL` for an empty interval or entirely zero-width
#' sweep. With `separate_caps = TRUE`, a list with `surface` and `caps` entries,
#' each containing a `ray_mesh` or `NULL`.
#' @seealso [extruded_polygon_mesh()], [lathe_mesh()]
#' @export
#' @examples
#' path = rbind(c(0, 0, 0), c(0, 1, 1), c(1, 2, 1))
#' tube = extruded_path_mesh(path, width = 0.2, smooth_normals = TRUE)
#' parts = extruded_path_mesh(path, width = 0.2, separate_caps = TRUE)
extruded_path_mesh = function(
  points,
  polygon = NA,
  polygon_end = NA,
  breaks = NA,
  closed = FALSE,
  closed_smooth = TRUE,
  polygon_add_points = 0,
  twists = 0,
  texture_repeats = 1,
  straight = FALSE,
  precomputed_control_points = FALSE,
  width = 1,
  width_end = NA,
  width_ease = "spline",
  smooth_normals = FALSE,
  u_min = 0,
  u_max = 1,
  linear_step = FALSE,
  end_caps = c(TRUE, TRUE),
  initial_normal = NULL,
  smooth_angle = 180,
  arc_tolerance = 1e-5,
  position = c(0, 0, 0),
  scale = c(1, 1, 1),
  angle = c(0, 0, 0),
  pivot_point = c(0, 0, 0),
  order_rotation = c(1, 2, 3),
  material = material_list(),
  material_caps = NULL,
  separate_caps = FALSE
) {
  if (
    !is.logical(separate_caps) ||
      length(separate_caps) != 1L ||
      is.na(separate_caps)
  ) {
    stop("`separate_caps` must be TRUE or FALSE.", call. = FALSE)
  }
  parts = sweep_mesh_data(
    points = points,
    polygon = polygon,
    polygon_end = polygon_end,
    breaks = breaks,
    closed = closed,
    closed_smooth = closed_smooth,
    polygon_add_points = polygon_add_points,
    twists = twists,
    texture_repeats = texture_repeats,
    straight = straight,
    precomputed_control_points = precomputed_control_points,
    width = width,
    width_end = width_end,
    width_ease = width_ease,
    smooth_normals = smooth_normals,
    u_min = u_min,
    u_max = u_max,
    linear_step = linear_step,
    end_caps = end_caps,
    initial_normal = initial_normal,
    smooth_angle = smooth_angle,
    arc_tolerance = arc_tolerance
  )
  if (is.null(material_caps)) {
    material_caps = material
  }
  group = function(pieces, mat) {
    pieces = Filter(Negate(is.null), pieces)
    if (!length(pieces)) {
      return(NULL)
    }
    meshes = lapply(pieces, function(piece) {
      do.call(construct_mesh, c(piece, list(material = mat)))
    })
    extrusion_transform(
      Reduce(add_shape, meshes),
      position,
      scale,
      angle,
      pivot_point,
      order_rotation
    )
  }
  surface = group(lapply(parts, function(part) part$surface), material)
  caps = group(
    unlist(lapply(parts, function(part) part$caps), recursive = FALSE),
    material_caps
  )
  if (separate_caps) {
    list(surface = surface, caps = caps)
  } else {
    add_shape(surface, caps)
  }
}

#' Apply standard transforms to an extrusion
#' @keywords internal
#' @noRd
extrusion_transform = function(
  mesh,
  position,
  scale,
  angle,
  pivot_point,
  order_rotation
) {
  for (name in c("position", "angle", "pivot_point")) {
    value = get(name)
    if (!is.numeric(value) || length(value) != 3L || any(!is.finite(value))) {
      stop(
        sprintf("`%s` must be a finite numeric three-vector.", name),
        call. = FALSE
      )
    }
  }
  if (
    !is.numeric(scale) ||
      !length(scale) %in% c(1L, 3L) ||
      any(!is.finite(scale)) ||
      any(scale == 0)
  ) {
    stop(
      "`scale` must contain one or three finite, nonzero numbers.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(order_rotation) ||
      length(order_rotation) != 3L ||
      anyNA(order_rotation) ||
      !setequal(order_rotation, 1:3)
  ) {
    stop("`order_rotation` must be a permutation of 1, 2, 3.", call. = FALSE)
  }
  if (any(scale != 1)) {
    mesh = scale_mesh(mesh, scale)
  }
  if (any(angle != 0)) {
    mesh = rotate_mesh(
      mesh,
      angle,
      pivot_point = pivot_point,
      order_rotation = order_rotation
    )
  }
  translate_mesh(mesh, position)
}
