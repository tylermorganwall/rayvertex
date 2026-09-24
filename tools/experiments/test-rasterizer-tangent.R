test_that("specialized tangent algebra preserves full shader results", {
  withr::local_options(cores = 4L)
  withr::local_envvar(c(
    RAYVERTEX_TANGENT_ALGEBRA = NA,
    RAYVERTEX_NORMAL_CACHE = NA
  ))
  normal = tempfile(fileext = ".ppm")
  withr::defer(unlink(normal))
  writeBin(
    c(
      charToRaw("P6\n2 2\n255\n"),
      as.raw(c(128, 128, 255, 180, 128, 240, 80, 128, 230, 128, 180, 240))
    ),
    normal
  )
  for (type in c("diffuse", "phong")) {
    for (uv in list(
      rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)),
      rbind(c(1, 0), c(0, 0), c(0, 1), c(1, 1)),
      matrix(0, 4, 2)
    )) {
      scene = construct_mesh(
        rbind(c(-1, -1, 0), c(1, -1, 0), c(1, 1, 0), c(-1, 1, 0.3)),
        rbind(c(0L, 1L, 2L), c(0L, 2L, 3L)),
        normals = rbind(c(0.1, 0.2, 2), c(-0.1, 0.3, 1)),
        norm_indices = rbind(c(0L, 1L, 0L), c(1L, 0L, 1L)),
        texcoords = uv,
        tex_indices = rbind(c(0L, 1L, 2L), c(0L, 2L, 3L)),
        material = material_list(
          type = type,
          normal_texture_location = normal,
          dissolve = 0.6,
          culling = "none"
        )
      )
      for (shadow in c(FALSE, TRUE)) {
        args = list(
          scene = scene,
          width = 47,
          height = 33,
          fsaa = 2,
          plot = FALSE,
          lookfrom = c(0, 0, 2),
          lookat = c(0, 0, 0),
          near_plane = 1.8,
          tangent_space_normals = TRUE,
          debug = "all",
          shadow_map = shadow,
          shadow_map_dims = c(19, 13),
          light_info = rbind(
            directional_light(c(1, 2, 3)),
            point_light(c(1, -1, 2), falloff = 0.2, falloff_quad = 0.1)
          )
        )
        Sys.unsetenv(c("RAYVERTEX_TANGENT_ALGEBRA", "RAYVERTEX_NORMAL_CACHE"))
        reference = do.call(rasterize_scene, args)
        Sys.setenv(RAYVERTEX_TANGENT_ALGEBRA = "1")
        expect_identical(do.call(rasterize_scene, args), reference)
        Sys.setenv(RAYVERTEX_NORMAL_CACHE = "1")
        args$scene = prepare_scene(scene)
        expect_identical(do.call(rasterize_scene, args), reference)
      }
    }
  }
})
