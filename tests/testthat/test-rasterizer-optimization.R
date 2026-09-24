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
