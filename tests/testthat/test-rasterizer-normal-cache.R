test_that("normal reuse retains shader conventions and independent indices", {
  withr::local_options(cores = 4L)
  withr::local_envvar(RAYVERTEX_NORMAL_CACHE = NA)
  normals = rbind(c(0.1, 0.2, 2), c(0, 1e-100, 3), c(0, 0, 0))
  for (type in c("vertex", "diffuse", "phong", "color", "toon", "toon_phong")) {
    for (geometric in c(FALSE, TRUE)) {
      indices = rbind(c(0L, 1L, 2L), c(0L, 2L, 3L))
      ni = rbind(c(0L, 1L, 0L), c(1L, 0L, 1L))
      if (geometric) {
        ni[1, ] = -1L
      }
      scene = construct_mesh(
        rbind(c(-1, -1, 0), c(1, -1, 0), c(1, 1, 0), c(-1, 1, 0)),
        indices,
        normals = normals,
        norm_indices = ni,
        material = material_list(type = type, culling = "none")
      )
      args = list(
        scene = scene,
        width = 47,
        height = 33,
        fsaa = 1,
        plot = FALSE,
        lookfrom = c(1, 2, 4),
        lookat = c(0, 0, 0),
        debug = "all",
        shadow_map_dims = c(19, 13)
      )
      Sys.unsetenv("RAYVERTEX_NORMAL_CACHE")
      reference = do.call(rasterize_scene, args)
      path = tempfile()
      Sys.setenv(RAYVERTEX_NORMAL_CACHE = "1")
      result = withr::with_envvar(
        c(RAYVERTEX_PROFILE = path),
        do.call(rasterize_scene, args)
      )
      expect_identical(result, reference)
      stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
      counts = setNames(stats$value, stats$name)
      expect_gt(counts[["count_normal_cache_hits"]], 0)
      expect_equal(counts[["count_normal_cache_misses"]], 2)
      expect_equal(
        counts[["count_geometric_normal_reuses"]],
        if (geometric) 2 else 0
      )
      expect_lte(counts[["count_normal_cache_payload_bytes"]], 64 * 1024^2)
      args$scene = prepare_scene(scene)
      expect_identical(do.call(rasterize_scene, args), reference)
      unlink(path)
    }
  }
})


test_that("mixed normal conventions share only identical cache keys", {
  withr::local_envvar(RAYVERTEX_NORMAL_CACHE = NA)
  scene = NULL
  for (type in c("diffuse", "phong", "toon_phong", "color")) {
    scene = add_shape(
      scene,
      sphere_mesh(material = material_list(type = type, dissolve = 0.4))
    )
  }
  args = list(
    scene = scene,
    width = 43,
    height = 29,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(1, 2, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    debug = "all"
  )
  reference = do.call(rasterize_scene, args)
  Sys.setenv(RAYVERTEX_NORMAL_CACHE = "1")
  expect_identical(do.call(rasterize_scene, args), reference)
  args$scene = prepare_scene(scene)
  expect_identical(do.call(rasterize_scene, args), reference)
})
