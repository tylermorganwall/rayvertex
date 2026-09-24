test_that("identical environment preparation shares variants across materials and background", {
  texture = tempfile(fileext = ".ppm")
  path = tempfile()
  on.exit(unlink(c(texture, path)))
  writeBin(c(charToRaw("P6\n8 4\n255\n"), as.raw((0:95 * 7) %% 256)), texture)
  scene = NULL
  for (i in 1:3) {
    scene = add_shape(
      scene,
      cube_mesh(
        position = c((i - 2) / 2, 0, 0),
        scale = 0.4,
        material = material_list(
          reflection_intensity = i / 4,
          reflection_sharpness = c(0.5, 0.51, 1)[i],
          ior = if (i == 2) 1.5 else 1
        )
      )
    )
  }
  args = list(
    scene = scene,
    width = 43,
    height = 31,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    environment_map = texture,
    background_sharpness = 0.5,
    debug = "all"
  )
  reference = do.call(rasterize_scene, c(args, list(parallel = FALSE)))
  withr::local_envvar(RAYVERTEX_PROFILE = path)
  withr::local_options(cores = 4L)
  expect_identical(do.call(rasterize_scene, args), reference)
  stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
  expect_equal(
    stats$value[stats$name == "count_environment_variant_requests"],
    4
  )
  expect_equal(
    stats$value[stats$name == "count_environment_resize_variants"],
    1
  )
  expect_equal(
    stats$value[stats$name == "count_environment_variant_bytes"],
    8 * 4 * 3 * 4
  )
})
