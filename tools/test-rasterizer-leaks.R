# Run with macOS leaks --atExit; ownership counters supplement the leak detector.
# Usage: Rscript tools/test-rasterizer-leaks.R LIB [ITERATIONS]
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
options(cores = 4L)
texture = tempfile(fileext = ".ppm")
bad = tempfile(fileext = ".ppm")
writeBin(
  c(charToRaw("P6\n32 16\n255\n"), as.raw(rep(c(90, 160, 220), 32 * 16))),
  texture
)
writeLines("invalid image", bad)
scene = sphere_mesh(
  material = material_list(
    texture_location = texture,
    ior = 1.5,
    reflection_intensity = 0,
    reflection_sharpness = 0.25
  )
)
broken = add_shape(
  scene,
  cube_mesh(material = material_list(texture_location = bad))
)
render = function(scene) {
  rasterize_scene(
    scene,
    width = 64,
    height = 48,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = TRUE,
    shadow_map_dims = c(31, 23),
    ssao = TRUE,
    environment_map = texture,
    background_sharpness = 0.5
  )
}
invisible(gc())
baseline = rayvertex:::prepared_scene_lifetime()
reference = render(scene)
for (i in seq_len(if (length(args) > 1) as.integer(args[2]) else 50L)) {
  prepared = prepare_scene(scene)
  stopifnot(identical(render(prepared), reference))
  failed = tryCatch(
    {
      update_prepared_scene(prepared, broken)
      FALSE
    },
    error = function(e) TRUE
  )
  stopifnot(failed, identical(render(prepared), reference))
  updated = update_prepared_scene(prepared, scene)
  stopifnot(identical(render(updated), reference))
  rm(prepared, updated)
  invisible(gc())
  stopifnot(identical(rayvertex:::prepared_scene_lifetime(), baseline))
}
unlink(c(texture, bad))
invisible(gc())
cat(
  "Prepared refresh, partial decode failure, refraction, shadow, SSAO: exact outputs and balanced ownership\n"
)
