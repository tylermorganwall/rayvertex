test_that("bounded parallel bins preserve main and shadow submission order", {
  side = 128L
  xy = expand.grid(
    x = seq(-1, 1, length.out = side + 1L),
    y = seq(-1, 1, length.out = side + 1L)
  )
  cells = expand.grid(x = 0:(side - 1L), y = 0:(side - 1L))
  a = as.integer(cells$x + (side + 1L) * cells$y)
  indices = rbind(
    cbind(a, a + 1L, a + side + 1L),
    cbind(a + 1L, a + side + 2L, a + side + 1L)
  )
  scene = construct_mesh(
    cbind(xy$x, xy$y, 0.05 * sin(9 * xy$x)),
    indices,
    material = material_list(culling = "none")
  )
  args = list(
    scene = scene,
    width = 31,
    height = 25,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    shadow_map = TRUE,
    shadow_map_dims = c(31, 23),
    debug = "all"
  )
  withr::local_envvar(RAYVERTEX_PARALLEL_BINS = NA)
  reference = do.call(rasterize_scene, c(args, list(parallel = FALSE)))
  withr::local_envvar(RAYVERTEX_PARALLEL_BINS = "1")
  for (cores in c(1L, 2L, 4L)) {
    withr::local_options(cores = cores)
    path = tempfile()
    withr::local_envvar(RAYVERTEX_PROFILE = path)
    expect_identical(do.call(rasterize_scene, args), reference)
    stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
    workers = stats$value[stats$name == "count_main_bin_workers"]
    expect_lte(workers, cores)
    if (cores > 1 && !nzchar(Sys.getenv("RAYVERTEX_TEST_NO_THREADS"))) {
      expect_gte(workers, 2)
    }
    expect_lte(
      stats$value[stats$name == "count_main_bin_scratch_bytes"],
      32 * 1024^2
    )
    unlink(path)
  }
  args$scene = prepare_scene(scene)
  expect_identical(do.call(rasterize_scene, args), reference)
})
