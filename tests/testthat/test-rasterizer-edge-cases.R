test_that("degenerate UVs and extreme valid cameras preserve scalar results", {
  texture = tempfile(fileext = ".ppm")
  writeBin(
    c(charToRaw("P6\n2 2\n255\n"), as.raw(rep(c(128, 128, 255), 4))),
    texture
  )
  on.exit(unlink(texture))
  withr::local_options(cores = 2L)
  switches = c(
    "RAYVERTEX_NORMAL_CACHE",
    "RAYVERTEX_BLOCK_COVERAGE",
    "RAYVERTEX_PARALLEL_BINS",
    "RAYVERTEX_VISIBILITY",
    "RAYVERTEX_MACROTILE_EDGE"
  )
  withr::local_envvar(setNames(rep(NA_character_, length(switches)), switches))
  vertices = rbind(c(-1, -1, 0), c(1, -1, 0), c(0, 1, 0))
  uvs = list(
    collapsed = matrix(0, 3, 2),
    collinear = cbind(0:2, 0:2),
    tiny = rbind(c(0, 0), c(1e-12, 0), c(0, 1e-12)),
    mirrored = rbind(c(0, 0), c(0, 1), c(1, 0))
  )
  cameras = list(
    narrow = list(fov = 0.01),
    wide = list(fov = 179),
    close = list(lookfrom = c(0, 0, 0.01)),
    distant = list(lookfrom = c(0, 0, 1e6)),
    orthographic = list(fov = 0, ortho_dimensions = c(0.01, 1000))
  )
  for (type in c("diffuse", "phong")) {
    for (uv in uvs) {
      mesh = construct_mesh(
        vertices,
        matrix(0:2, 1),
        texcoords = uv,
        tex_indices = matrix(0:2, 1),
        normals = matrix(c(0, 0, 1), 1),
        norm_indices = matrix(0L, 1, 3),
        material = material_list(
          type = type,
          culling = "none",
          normal_texture_location = texture
        )
      )
      for (camera in cameras) {
        args = modifyList(
          list(
            scene = mesh,
            width = 17,
            height = 13,
            fsaa = 1,
            plot = FALSE,
            lookfrom = c(0, 0, 4),
            lookat = c(0, 0, 0),
            shadow_map = FALSE,
            debug = "all",
            parallel = FALSE
          ),
          camera
        )
        Sys.unsetenv(switches)
        reference = suppressWarnings(do.call(rasterize_scene, args))
        Sys.setenv(
          RAYVERTEX_NORMAL_CACHE = "1",
          RAYVERTEX_BLOCK_COVERAGE = "1",
          RAYVERTEX_PARALLEL_BINS = "1",
          RAYVERTEX_VISIBILITY = "1",
          RAYVERTEX_MACROTILE_EDGE = "32"
        )
        args$scene = prepare_scene(mesh)
        args$parallel = TRUE
        result = suppressWarnings(do.call(rasterize_scene, args))
        # Singular tangent inputs retain their existing exceptional-value contract.
        expect_identical(result, reference)
        expect_true(all(is.finite(result$depth)))
        expect_true(all(result$depth >= -1 & result$depth <= 1))
      }
    }
  }
})
