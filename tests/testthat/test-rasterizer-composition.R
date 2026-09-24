test_that("fused composition preserves operation order and input ownership", {
  withr::local_envvar(RAYVERTEX_REFERENCE_OUTPUT = NA)
  values = c(-Inf, -1, -0, 0, 0.04045, 0.5, 1, 3, Inf, NA, NaN)
  buffers = setNames(
    lapply(1:4, function(i) matrix(rep(values, length.out = 15), 3, 5)),
    c("r", "g", "b", "a")
  )
  buffers$depth = matrix(rep(c(0.5, 1), length.out = 15), 3, 5)
  snapshot = unserialize(serialize(buffers, NULL))
  for (ambient in list(NULL, matrix(rep(c(0, 1, Inf, NaN, -2), 3), 3, 5))) {
    for (bg in list(NULL, c(0.1, 0.3, 0.9), c(NA, Inf, -1))) {
      reference = withr::with_envvar(
        c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
        rayvertex:::raster_output_image(
          buffers,
          background = bg,
          ambient = ambient
        )
      )
      expect_identical(
        rayvertex:::raster_output_image(
          buffers,
          background = bg,
          ambient = ambient
        ),
        reference
      )
      expect_identical(buffers, snapshot)
    }
  }
})

test_that("composition fusion preserves public effects and debug-all", {
  withr::local_envvar(RAYVERTEX_REFERENCE_COMPOSITION = NA)
  withr::local_options(cores = 4L)
  for (ssao in c(FALSE, TRUE)) {
    for (transparent in c(FALSE, TRUE)) {
      for (debug in c("none", "all")) {
        args = list(
          scene = sphere_mesh(),
          width = 51,
          height = 37,
          fsaa = 2,
          plot = FALSE,
          lookfrom = c(0, 0, 4),
          lookat = c(0, 0, 0),
          ssao = ssao,
          ssao_intensity = 2.5,
          background = "#17395B",
          transparent_background = transparent,
          shadow_map_dims = c(19, 13),
          bloom = TRUE,
          tonemap = "reinhard",
          debug = debug
        )
        reference = withr::with_envvar(
          c(RAYVERTEX_REFERENCE_COMPOSITION = "1"),
          do.call(rasterize_scene, args)
        )
        expect_identical(do.call(rasterize_scene, args), reference)
        args$scene = prepare_scene(args$scene)
        expect_identical(do.call(rasterize_scene, args), reference)
      }
    }
  }
})
