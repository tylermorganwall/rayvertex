# rayvertex 0.16.0

* Added `extruded_path_mesh()` and `extruded_polygon_mesh()`, moving sweep and
  polygon construction from rayrender into rayvertex. Both return standard
  `ray_mesh` objects and support rayvertex materials and transforms.
* Preserved the corrected sweep seams, taper/twist normals, exact trim endpoints,
  profile morphing, cap choices, polygon winding, holes, spatial input, and
  per-feature height handling. Geometry regression tests now live in rayvertex.
* Added `lathe_mesh()` for surfaces of revolution, with welded angular seams,
  nondegenerate axis endpoints, profile normals, and independent UV seams.
