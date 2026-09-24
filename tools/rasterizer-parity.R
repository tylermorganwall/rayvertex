# Broad, small deterministic image/depth corpus, kept separate from benchmarks.
# Usage: Rscript tools/rasterizer-parity.R LIB OUTPUT_RDS [REFERENCE_RDS]
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
source("tools/rasterizer-fixtures.R")
options(cores = 4L)
base = list(
  width = 47,
  height = 33,
  fsaa = 1,
  plot = FALSE,
  lookfrom = c(0, 0, 4),
  lookat = c(0, 0, 0),
  fov = 40,
  shadow_map = FALSE,
  shadow_map_dims = c(41, 29)
)
cases = list()
for (type in c("vertex", "diffuse", "phong", "color", "toon", "toon_phong")) {
  for (shadow in c(FALSE, TRUE)) {
    cases[[paste(type, shadow)]] = list(
      scene = sphere_mesh(material = material_list(type = type)),
      shadow_map = shadow
    )
  }
}
cases$ssao = list(scene = sphere_mesh(), ssao = TRUE)
cases$orthographic = list(
  scene = sphere_mesh(),
  fov = 0,
  ortho_dimensions = c(3, 2)
)
cases$fsaa2 = list(scene = cube_mesh(), fsaa = 2)
cases$portrait = list(scene = cube_mesh(), width = 31, height = 49)
cases$empty = list(
  scene = construct_mesh(matrix(c(0, 0, 0), 1), matrix(integer(), 0, 3))
)
cases$offscreen = list(scene = sphere_mesh(position = c(100, 0, 0)))
cases$near_crossing = list(
  scene = construct_mesh(
    rbind(c(-0.2, -0.2, 3.95), c(0.4, -0.2, 3), c(0, 0.4, 3)),
    matrix(0:2, 1),
    material = material_list(culling = "none")
  )
)
cases$sliver = list(
  scene = construct_mesh(
    rbind(c(-1, 0, 0), c(1, 0, 0), c(0, 1e-5, 0)),
    matrix(0:2, 1),
    material = material_list(culling = "none")
  )
)
for (layers in c(4, 16, 64, 129)) {
  cases[[paste0("alpha", layers)]] = list(
    scene = rasterizer_grid(512, layers, 0.1),
    shadow_map = TRUE
  )
}
cases$lights = list(
  scene = sphere_mesh(),
  shadow_map = TRUE,
  light_info = add_light(
    directional_light(c(1, 2, 3)),
    directional_light(c(-1, 2, 3))
  )
)
lines = rbind(
  c(-0.8, -0.8, 1, 0.8, 0.8, 1, 1, 0, 0),
  c(0.7, -0.8, 1, 0.7, 0.8, 1, 0, 1, 0),
  c(-0.6, 0.8, 1, -0.7, -0.8, 1, 0, 0, 1),
  c(0.8, 0, 1, -0.8, 0, 1, 1, 1, 0)
)
for (aa in c(FALSE, TRUE)) {
  for (portrait in c(FALSE, TRUE)) {
    cases[[paste("lines", aa, portrait)]] = list(
      scene = cube_mesh(),
      line_info = lines,
      antialias_lines = aa,
      alpha_line = 0.4,
      width = if (portrait) 31 else 47,
      height = if (portrait) 49 else 33
    )
  }
}
# Independent normal/UV indices and a deterministic 2x2 local normal map.
texture = tempfile(fileext = ".ppm")
writeBin(
  c(charToRaw("P6\n2 2\n255\n"), as.raw(rep(c(128, 128, 255), 4))),
  texture
)
quad = construct_mesh(
  rbind(c(-1, -1, 0), c(1, -1, 0), c(1, 1, 0), c(-1, 1, 0)),
  rbind(c(0, 1, 2), c(0, 2, 3)),
  normals = rbind(c(0, 0, 1), c(0.1, 0, 0.99)),
  norm_indices = rbind(c(0, 1, 0), c(1, 0, 1)),
  texcoords = rbind(c(-0.3, 0), c(1.7, 0), c(1, 1), c(0, 1)),
  tex_indices = rbind(c(0, 1, 2), c(2, 0, 3)),
  material = material_list(normal_texture_location = texture, culling = "none")
)
cases$tangent_diffuse = list(scene = quad)
cases$tangent_phong = list(scene = change_material(quad, type = "phong"))
cases$refraction = list(
  scene = sphere_mesh(material = material_list(ior = 1.5)),
  environment_map = texture,
  background_sharpness = 0.5
)
results = list()
for (name in names(cases)) {
  params = modifyList(base, cases[[name]])
  for (debug in c(
    "none",
    "all",
    "raw_depth",
    "normals",
    "uv",
    "position",
    "depth"
  )) {
    key = paste(name, debug, sep = "/")
    results[[key]] = suppressWarnings(do.call(
      rasterize_scene,
      c(params, list(debug = debug))
    ))
  }
}
unlink(texture)
saveRDS(results, args[2])
if (length(args) >= 3L) {
  reference = readRDS(args[3])
  failures = names(results)[
    !vapply(
      names(results),
      function(n) identical(results[[n]], reference[[n]]),
      TRUE
    )
  ]
  if (length(failures)) {
    stop("Parity failures: ", paste(failures, collapse = ", "))
  }
}
cat(length(results), "render/debug results passed\n")
