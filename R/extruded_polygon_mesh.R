#' Extruded Polygon Mesh
#'
#' Build polygon caps and walls with shared geometric vertices, including
#' multiple holes, spatial features, and multipart polygons.
#'
#' @param polygon Default `NULL`. Coordinates accepted by `xy.coords()`, an
#' `sf` object containing POLYGON or MULTIPOLYGON features, or a
#' `SpatialPolygons`/`SpatialPolygonsDataFrame` object. Open rings are closed;
#' consecutive duplicate and redundant collinear vertices are removed. Rings
#' must be simple with nonzero area. Holes must be strictly inside their exterior
#' and must not touch or overlap. Z and M coordinates are ignored.
#' @param plane Default `"xz"`. Coordinate-plane orientation: `xz`, `zx`, `xy`,
#' `yx`, `yz`, or `zy`, matched without regard to case. Preserves rayrender's
#' historical coordinate permutation and first-coordinate mirror.
#' @param top Default `1`. Finite extrusion height. Either height order is
#' supported with outward-facing triangles. Equal heights produce one flat cap.
#' @param bottom Default `0`. Finite starting height of the extrusion.
#' @param holes Default `NULL`. For direct coordinates, strictly increasing
#' one-based start indices of the holes in the original input, before cleanup.
#' `NULL` or `0` means no holes. Spatial inputs determine holes from their rings.
#' @param center Default `FALSE`. Center the combined polygon extent before
#' applying the plane mapping and mesh transforms.
#' @param flip_horizontal Default `FALSE`. Flip the first polygon coordinate.
#' @param flip_vertical Default `FALSE`. Flip the second polygon coordinate.
#' @param data_column_top Default `NULL`. Numeric `sf` column supplying each
#' feature's top height. A missing column warns and uses `top`.
#' @param data_column_bottom Default `NULL`. Numeric `sf` column supplying each
#' feature's bottom height. A missing column warns and uses `bottom`.
#' @param scale_data Default `1`. Scale for heights read from data columns.
#' @param position Default `c(0, 0, 0)`. Translation of the completed mesh.
#' @param scale Default `c(1, 1, 1)`. Finite, nonzero mesh scale, uniformly or
#' per axis. Reflected scales preserve outward triangle winding.
#' @param angle Default `c(0, 0, 0)`. Rotation angles in degrees.
#' @param pivot_point Default `c(0, 0, 0)`. Point around which to rotate.
#' @param order_rotation Default `c(1, 2, 3)`. Order to rotate the axes.
#' @param material Default `material_list()`. Mesh material.
#'
#' @return A `ray_mesh` containing all polygon components. Equal-height features
#' contribute flat surfaces, which cannot enclose participating media.
#' @seealso [extruded_path_mesh()], [lathe_mesh()]
#' @export
#' @examples
#' outline = rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1))
#' block = extruded_polygon_mesh(outline, top = 2)
#' hole = outline * 0.3
#' frame = extruded_polygon_mesh(rbind(outline, hole), holes = 5, top = 0.5)
extruded_polygon_mesh = function(
  polygon = NULL,
  plane = "xz",
  top = 1,
  bottom = 0,
  holes = NULL,
  center = FALSE,
  flip_horizontal = FALSE,
  flip_vertical = FALSE,
  data_column_top = NULL,
  data_column_bottom = NULL,
  scale_data = 1,
  position = c(0, 0, 0),
  scale = c(1, 1, 1),
  angle = c(0, 0, 0),
  pivot_point = c(0, 0, 0),
  order_rotation = c(1, 2, 3),
  material = material_list()
) {
  data = polygon_mesh_data(
    polygon,
    plane,
    top,
    bottom,
    holes,
    center,
    flip_horizontal,
    flip_vertical,
    data_column_top,
    data_column_bottom,
    scale_data,
    scale
  )
  mesh = construct_mesh(data$vertices, data$indices, material = material)
  extrusion_transform(mesh, position, 1, angle, pivot_point, order_rotation)
}
