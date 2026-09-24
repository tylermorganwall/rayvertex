# rayvertex 0.16.1

* Added `prepare_scene()` and `update_prepared_scene()` for explicit reusable
  geometry/material-texture snapshots. Camera, lighting, callbacks, effects,
  shadows, and environment preparation remain per render.
* Corrected homogeneous triangle clipping and orthographic/shadow depth ranges.
  Near-plane intersections, geometry outside the frustum, and orthographic depth
  can change. Extreme tile bounds and nonfinite area reciprocals are guarded.
* Added shared triangle setup, contiguous bins, unlimited sparse transparency,
  demand-driven auxiliary buffers, parallel resolve/background/outline work,
  exact shadow/light specialization, shared environment variants, stable geometry
  views, and one SSAO projection composition per frame. The conservative visibility
  experiment remains disabled by default. Per-phase timings, memory, correctness
  changes, regressions, and exact image comparisons are in `docs/performance/`.

* Fixed SSAO indexing and blur ownership, rectangular unaliased line indexing,
  grayscale texture sampling, shader initialization, and environment image
  lifetime. SSAO images can change as a result of these correctness fixes.
* Reduced rasterizer allocations, batched active coverage blocks, shared texture
  decoding within each render, and parallelized SSAO and toon outline propagation.
  Rendering settings and public R interfaces are preserved. Developer benchmarks,
  a corrected scalar reference, and measured limitations are documented in
  `docs/performance/` in the source repository.

* Added `draw_lathe_profile()` to draw radius/height polygons with base R
  mouse input, a live closing-edge preview, an Undo button, and profiles ready
  for `lathe_mesh()`.


# rayvertex 0.16.0

* Added `extruded_path_mesh()` and `extruded_polygon_mesh()`, moving sweep and
  polygon construction from rayrender into rayvertex. Both return standard
  `ray_mesh` objects and support rayvertex materials and transforms.
* Preserved the corrected sweep seams, taper/twist normals, exact trim endpoints,
  profile morphing, cap choices, polygon winding, holes, spatial input, and
  per-feature height handling. Geometry regression tests now live in rayvertex.
* Added `lathe_mesh()` for surfaces of revolution, with welded angular seams,
  nondegenerate axis endpoints, profile normals, and independent UV seams.
