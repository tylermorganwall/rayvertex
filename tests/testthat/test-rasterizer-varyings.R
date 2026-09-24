test_that("common varyings are retained only for their actual consumers", {
  withr::local_options(cores = 4L)
  withr::local_envvar(RAYVERTEX_REFERENCE_VARYINGS = NA)
  withr::local_envvar(RAYVERTEX_INDEXED_TRANSFORMS = NA)
  texture = tempfile(fileext = ".ppm")
  writeBin(
    c(charToRaw("P6\n2 2\n255\n"), as.raw(rep(c(128, 128, 255), 4))),
    texture
  )
  withr::defer(unlink(texture))
  base = list(
    width = 47,
    height = 33,
    fsaa = 1,
    plot = FALSE,
    shadow_map = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0)
  )
  cases = list(
    plain = list(),
    uv = list(debug = "uv"),
    position = list(debug = "position"),
    all = list(debug = "all"),
    ssao = list(ssao = TRUE),
    shadow = list(shadow_map = TRUE, shadow_map_dims = c(19, 13)),
    point = list(light_info = point_light(c(1, 2, 3)))
  )
  for (sigma in c(1, 45, 90)) {
    cases[[paste0("oren", sigma)]] = list(
      scene = sphere_mesh(material = material_list(sigma = sigma))
    )
  }
  for (type in c("vertex", "diffuse", "phong", "color", "toon", "toon_phong")) {
    cases[[type]] = list(
      scene = sphere_mesh(material = material_list(type = type))
    )
  }
  for (channel in c(
    "texture_location",
    "ambient_texture_location",
    "specular_texture_location",
    "emissive_texture_location",
    "normal_texture_location"
  )) {
    material = do.call(material_list, setNames(list(texture), channel))
    cases[[channel]] = list(scene = sphere_mesh(material = material))
  }
  cases$reflection = list(
    scene = sphere_mesh(material = material_list(reflection_intensity = 0.5)),
    environment_map = texture
  )
  cases$refraction = list(
    scene = sphere_mesh(material = material_list(ior = 1.5)),
    environment_map = texture
  )
  cases$mixed = list(
    scene = add_shape(
      sphere_mesh(
        position = c(-0.5, 0, 0),
        material = material_list(sigma = 45)
      ),
      cube_mesh(
        position = c(0.5, 0, 0),
        material = material_list(texture_location = texture)
      )
    )
  )
  for (name in names(cases)) {
    params = modifyList(base, cases[[name]])
    if (is.null(params$scene)) {
      params$scene = sphere_mesh()
    }
    path = tempfile()
    withr::local_envvar(RAYVERTEX_PROFILE = path)
    Sys.setenv(RAYVERTEX_REFERENCE_VARYINGS = "1")
    reference = do.call(rasterize_scene, params)
    unlink(path)
    Sys.unsetenv("RAYVERTEX_REFERENCE_VARYINGS")
    expect_identical(do.call(rasterize_scene, params), reference, info = name)
    stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
    bytes = setNames(stats$value, stats$name)
    retained = c(
      uv = bytes[["count_varying_uv_payload_bytes"]],
      position = bytes[["count_varying_position_payload_bytes"]],
      clip = bytes[["count_varying_clip_payload_bytes"]]
    )
    expected = c(
      uv = name %in% c("uv", "all", "mixed") || grepl("texture_location", name),
      position = name %in%
        c(
          "position",
          "all",
          "ssao",
          "point",
          "reflection",
          "refraction",
          "mixed"
        ) ||
        grepl("oren", name),
      clip = name == "shadow"
    )
    expect_identical(retained > 0, expected, info = name)
    params$scene = prepare_scene(params$scene)
    expect_identical(
      do.call(rasterize_scene, params),
      reference,
      info = paste(name, "prepared")
    )
    Sys.setenv(RAYVERTEX_INDEXED_TRANSFORMS = "1")
    expect_identical(
      do.call(rasterize_scene, params),
      reference,
      info = paste(name, "indexed")
    )
    Sys.unsetenv("RAYVERTEX_INDEXED_TRANSFORMS")
    unlink(path)
  }
})
