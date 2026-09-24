test_that("Oren-Nayar snapshots preserve sidedness and mixed-light shadow results", {
  withr::local_options(cores = 4L)
  lights = rbind(
    directional_light(c(1, 2, 3)),
    directional_light(c(-1, 1, 2)),
    point_light(c(1, -1, 2), falloff = 0.2, falloff_quad = 0.1)
  )
  for (sigma in c(1, 45, 90)) {
    for (two_sided in c(FALSE, TRUE)) {
      scene = sphere_mesh(
        material = material_list(
          sigma = sigma,
          two_sided = two_sided
        )
      )
      args = list(
        scene = scene,
        width = 43,
        height = 31,
        fsaa = 1,
        plot = FALSE,
        lookfrom = c(0, 0, 4),
        lookat = c(0, 0, 0),
        light_info = lights,
        shadow_map = TRUE,
        shadow_map_dims = c(7, 5),
        debug = "all"
      )
      reference = do.call(rasterize_scene, c(args, list(parallel = FALSE)))
      expect_true(all(is.finite(reference$r)))
      expect_true(any(reference$depth < 1))
      args$scene = prepare_scene(scene)
      expect_identical(do.call(rasterize_scene, args), reference)
    }
  }
})
