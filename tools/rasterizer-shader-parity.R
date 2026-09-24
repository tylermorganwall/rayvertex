# Additional shader coverage beyond the retained 245-result corpus.
# Usage: Rscript tools/rasterizer-shader-parity.R LIB OUTPUT_RDS [REFERENCE_RDS]
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
options(cores = 4L)
texture = tempfile(fileext = ".tga")
normal = tempfile(fileext = ".ppm")
header = integer(18)
header[3] = 2L
header[13] = header[15] = 2L
header[17] = 32L
header[18] = 8L
writeBin(
  as.raw(c(
    header,
    30,
    90,
    230,
    0,
    210,
    80,
    60,
    96,
    30,
    210,
    70,
    192,
    180,
    90,
    220,
    255
  )),
  texture
)
writeBin(
  c(
    charToRaw("P6\n2 2\n255\n"),
    as.raw(c(128, 128, 255, 180, 128, 240, 80, 128, 230, 128, 180, 240))
  ),
  normal
)
lights = rbind(
  directional_light(c(1, 2, 3)),
  directional_light(c(-1, 1, 2)),
  point_light(c(1, -1, 2), falloff = 0.2, falloff_quad = 0.1)
)
cases = list()
for (type in c("vertex", "diffuse", "phong", "color", "toon", "toon_phong")) {
  cases[[type]] = list(
    scene = sphere_mesh(material = material_list(type = type)),
    light_info = lights
  )
  cases[[paste0(type, "_alpha")]] = list(
    scene = sphere_mesh(
      material = material_list(type = type, texture_location = texture)
    ),
    light_info = lights
  )
}
for (two_sided in c(FALSE, TRUE)) {
  for (sigma in c(1, 45, 90)) {
    cases[[paste("oren", sigma, two_sided)]] = list(
      scene = sphere_mesh(
        material = material_list(
          sigma = sigma,
          two_sided = two_sided,
          texture_location = texture
        )
      ),
      light_info = lights
    )
  }
}
for (type in c("diffuse", "phong")) {
  for (tangent in c(FALSE, TRUE)) {
    cases[[paste(type, "normal", tangent)]] = list(
      scene = sphere_mesh(
        material = material_list(type = type, normal_texture_location = normal)
      ),
      light_info = lights,
      tangent_space_normals = tangent
    )
  }
}
result = list()
for (name in names(cases)) {
  for (shadow in c(FALSE, TRUE)) {
    params = modifyList(
      list(
        width = 59,
        height = 37,
        fsaa = 2,
        plot = FALSE,
        lookfrom = c(0, 0, 4),
        lookat = c(0, 0, 0),
        shadow_map = shadow,
        shadow_map_dims = c(7, 5),
        debug = "all"
      ),
      cases[[name]]
    )
    if (Sys.getenv("RAYVERTEX_PARITY_PREPARED") == "1") {
      params$scene = prepare_scene(params$scene)
    }
    result[[paste(name, shadow)]] = do.call(rasterize_scene, params)
  }
}
saveRDS(result, args[2])
unlink(c(texture, normal))
if (length(args) >= 3) {
  stopifnot(identical(result, readRDS(args[3])))
}
cat(length(result), "extended shader/debug results passed\n")
