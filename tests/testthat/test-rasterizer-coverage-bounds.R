test_that("block extrema retain inclusive edges, winding, culling and debug buffers", {
  withr::local_options(cores = 4L)
  withr::local_envvar(RAYVERTEX_BLOCK_COVERAGE = NA)
  for (culling in c("none", "front", "back")) {
    for (reverse in c(FALSE, TRUE)) {
      vertices = rbind(c(-1, -1, 0), c(1, -1, 0), c(-1, 1, 0))
      face = if (reverse) c(0L, 2L, 1L) else 0:2
      scene = construct_mesh(
        vertices,
        matrix(face, 1),
        material = material_list(culling = culling)
      )
      args = list(
        scene = scene,
        width = 61,
        height = 47,
        fsaa = 1,
        plot = FALSE,
        lookfrom = c(0, 0, 4),
        lookat = c(0, 0, 0),
        fov = 40,
        shadow_map_dims = c(31, 23),
        debug = "all"
      )
      for (block in c(1L, 4L, 7L, 16L)) {
        args$block_size = block
        Sys.unsetenv("RAYVERTEX_BLOCK_COVERAGE")
        reference = do.call(rasterize_scene, args)
        Sys.setenv(RAYVERTEX_BLOCK_COVERAGE = "1")
        path = tempfile()
        withr::local_envvar(RAYVERTEX_PROFILE = path)
        expect_identical(do.call(rasterize_scene, args), reference)
        if (culling == "none" && block == 4L) {
          stats = read.csv(path, header = FALSE, col.names = c("name", "value"))
          expect_gt(stats$value[stats$name == "count_coverage_full_blocks"], 0)
          expect_gt(
            stats$value[stats$name == "count_coverage_rejected_blocks"],
            0
          )
          expect_lt(
            stats$value[stats$name == "count_coverage_edge_samples"],
            stats$value[stats$name == "count_coverage_candidates"]
          )
        }
        unlink(path)
      }
    }
  }
})
