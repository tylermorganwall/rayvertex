test_that("spatial macrotiles preserve existing microblock coverage and effects", {
  withr::local_options(cores = 4L)
  withr::local_envvar(RAYVERTEX_MACROTILE_EDGE = NA)
  scene = add_shape(
    sphere_mesh(material = material_list(type = "toon")),
    cube_mesh(
      position = c(0.5, 0, 0.5),
      material = material_list(dissolve = 0.35)
    )
  )
  args = list(
    scene = scene,
    width = 47,
    height = 35,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    ssao = TRUE,
    shadow_map_dims = c(31, 23),
    debug = "all"
  )
  for (block in c(3L, 4L, 7L)) {
    args$block_size = block
    reference = do.call(rasterize_scene, c(args, list(parallel = FALSE)))
    for (edge in c(16L, 32L, 64L)) {
      withr::local_envvar(RAYVERTEX_MACROTILE_EDGE = as.character(edge))
      path = tempfile()
      withr::local_envvar(RAYVERTEX_PROFILE = path)
      expect_identical(do.call(rasterize_scene, args), reference)
      stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
      if (!nzchar(Sys.getenv("RAYVERTEX_TEST_NO_THREADS"))) {
        expect_gt(stats$value[stats$name == "count_main_macrotiles"], 0)
        expect_equal(
          stats$value[stats$name == "count_main_macrotile_edge"],
          edge
        )
      }
      unlink(path)
    }
  }
  withr::local_envvar(RAYVERTEX_MACROTILE_EDGE = "invalid")
  expect_error(do.call(rasterize_scene, args), "RAYVERTEX_MACROTILE_EDGE")
})
