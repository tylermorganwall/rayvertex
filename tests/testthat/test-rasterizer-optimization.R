test_that("material deduplication preserves first occurrence and default mapping", {
  for (hashes in list(c("a", "b", "a", "c", "b"), c("a", "b"), character())) {
    ids = if (length(hashes)) c(-1L, seq_along(hashes) - 1L) else integer()
    scene = list(
      materials = lapply(seq_along(hashes), function(i) list(id = i)),
      shapes = list(list(material_ids = ids))
    )
    attr(scene, "material_hashes") = hashes
    result = rayvertex:::remove_duplicate_materials(scene)
    expect_identical(attr(result, "material_hashes"), unique(hashes))
    expect_identical(
      result$materials,
      scene$materials[match(unique(hashes), hashes)]
    )
    ids[ids == -1L] = 0L
    expect_identical(
      result$shapes[[1]]$material_ids,
      (match(hashes, unique(hashes)) - 1L)[ids + 1L]
    )
  }
})

test_that("disabled shadow and outline passes allocate no pass-specific payload", {
  path = tempfile()
  withr::local_envvar(RAYVERTEX_PROFILE = path)
  rasterize_scene(
    cube_mesh(material = material_list(type = "phong")),
    width = 19,
    height = 27,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE
  )
  stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
  expect_equal(
    stats$value[stats$name == "count_outline_scratch_payload_bytes"],
    0
  )
  expect_equal(
    stats$value[stats$name == "count_shadow_matrix_payload_bytes"],
    0
  )
})

test_that("equal-depth surfaces and line ordering are deterministic across workers", {
  a = xy_rect_mesh(
    material = material_list(diffuse = "red", culling = "none", type = "color")
  )
  b = xy_rect_mesh(
    material = material_list(diffuse = "blue", culling = "none", type = "color")
  )
  for (alpha in c(1, 0.4)) {
    scene = add_shape(a, b)
    if (alpha < 1) {
      scene = change_material(scene, dissolve = alpha)
    }
    params = list(
      scene = scene,
      width = 37,
      height = 25,
      fsaa = 1,
      plot = FALSE,
      lookfrom = c(0, 0, 4),
      lookat = c(0, 0, 0),
      shadow_map = FALSE,
      debug = "all"
    )
    reference = do.call(rasterize_scene, c(params, list(parallel = FALSE)))
    for (cores in c(2L, 4L)) {
      withr::local_options(cores = cores)
      expect_identical(do.call(rasterize_scene, params), reference)
    }
  }
})

test_that("active block batches preserve corrected scalar coverage and ties", {
  scene = add_shape(cube_mesh(), sphere_mesh(position = c(0.8, 0, 0)))
  args = list(
    scene = scene,
    width = 37,
    height = 29,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    debug = "all",
    shadow_map_dims = c(33, 25)
  )
  withr::local_options(cores = 2L)
  withr::local_envvar(RAYVERTEX_REFERENCE_SCHEDULER = "1")
  reference = do.call(rasterize_scene, args)
  Sys.unsetenv("RAYVERTEX_REFERENCE_SCHEDULER")
  for (size in c(1, 16, 64, 256)) {
    withr::local_envvar(RAYVERTEX_BATCH_BLOCKS = as.character(size))
    expect_identical(do.call(rasterize_scene, args), reference)
  }
})

test_that("main and shadow shaders share decoded canonical texture identities", {
  texture = tempfile(fileext = ".ppm")
  writeBin(c(charToRaw("P6\n1 1\n255\n"), as.raw(c(90, 160, 220))), texture)
  on.exit(unlink(texture))
  alias = file.path(dirname(texture), ".", basename(texture))
  scene = add_shape(
    cube_mesh(material = material_list(texture_location = texture)),
    cube_mesh(
      position = c(0.5, 0, 0),
      material = material_list(
        texture_location = alias,
        diffuse_intensity = 0.5
      )
    )
  )
  path = tempfile()
  withr::local_envvar(RAYVERTEX_PROFILE = path)
  rasterize_scene(
    scene,
    width = 23,
    height = 19,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map_dims = c(17, 19)
  )
  stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
  expect_equal(stats$value[stats$name == "count_texture_decodes"], 1)
  expect_equal(stats$value[stats$name == "count_texture_payload_bytes"], 12)
})

test_that("a failed later decode releases earlier shader assets", {
  valid = tempfile(fileext = ".ppm")
  invalid = tempfile(fileext = ".tga")
  writeBin(c(charToRaw("P6\n1 1\n255\n"), as.raw(c(128, 128, 255))), valid)
  writeBin(charToRaw("not an image"), invalid)
  on.exit(unlink(c(valid, invalid)))
  scene = sphere_mesh(
    material = material_list(
      texture_location = valid,
      normal_texture_location = invalid
    )
  )
  for (i in 1:3) {
    expect_error(
      rasterize_scene(
        scene,
        width = 11,
        height = 13,
        fsaa = 1,
        plot = FALSE,
        lookat = c(0, 0, 0),
        shadow_map = FALSE
      ),
      "texture loading failed"
    )
  }
})

test_that("coverage counters reconcile on a deliberately simple quad", {
  scene = construct_mesh(
    rbind(c(-1, -1, 0), c(1, -1, 0), c(1, 1, 0), c(-1, 1, 0)),
    rbind(c(0, 1, 2), c(0, 2, 3)),
    material = material_list(type = "color", culling = "none")
  )
  path = tempfile()
  withr::local_envvar(RAYVERTEX_PROFILE = path)
  rasterize_scene(
    scene,
    width = 9,
    height = 9,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    fov = 0,
    ortho_dimensions = c(2, 2),
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE
  )
  stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
  counts = setNames(stats$value, stats$name)
  # Inclusive shared diagonal: eight of the 64 covered pixels are visited twice.
  expect_equal(unname(counts["count_covered_samples"]), 72)
  expect_equal(unname(counts["count_shader_calls"]), 72)
  expect_equal(unname(counts["count_early_z_failures"]), 0)
  expect_equal(unname(counts["count_transparent_fragments"]), 0)
})

test_that("parallel SSAO and jump-flood iterations match the scalar screen passes", {
  withr::local_options(cores = 4L)
  scene = sphere_mesh(material = material_list(type = "toon"))
  args = list(
    scene = scene,
    width = 127,
    height = 91,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    ssao = TRUE,
    debug = "all"
  )
  withr::local_envvar(RAYVERTEX_REFERENCE_SCREEN = "1")
  reference = do.call(rasterize_scene, args)
  Sys.unsetenv("RAYVERTEX_REFERENCE_SCREEN")
  expect_identical(do.call(rasterize_scene, args), reference)
})
