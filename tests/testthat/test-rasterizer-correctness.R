render_regression = function(scene = cube_mesh(), ...) {
  rasterize_scene(
    scene,
    width = 31,
    height = 23,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    ...
  )
}

test_that("presentation depth preserves the legacy NDC and background contract", {
  buffers = render_regression(debug = "all")
  background = buffers$normalx == 0 &
    buffers$normaly == 0 &
    buffers$normalz == 0
  expect_true(any(background))
  expect_true(all(buffers$depth[background] == 1))
  expect_true(all(is.finite(buffers$linear_depth)))
  expect_true(all(buffers$depth[!background] >= -1))
  expect_true(all(buffers$depth[!background] <= 1))
  expect_equal(dim(buffers$r), c(31L, 23L))
})

test_that("SSAO is deterministic across repeated renders", {
  a = render_regression(sphere_mesh(), debug = "all", ssao = TRUE)
  b = render_regression(sphere_mesh(), debug = "all", ssao = TRUE)
  expect_identical(a, b)
  expect_true(all(is.finite(a$amb)))
  expect_true(all(a$amb >= 0 & a$amb <= 1))
})

test_that("steep unaliased lines stay at the same x on rectangular images", {
  # Project a vertical line near the right boundary, safely in front of the cube.
  line = matrix(c(0.65, -0.6, 1, 0.65, 0.6, 1, 1, 0, 0), nrow = 1)
  out = render_regression(
    line_info = line,
    antialias_lines = FALSE,
    debug = "all"
  )
  line_pixels = out$r > 0.99 & out$g == 0 & out$b == 0
  expect_gt(sum(line_pixels), 5)
  expect_length(unique(which(line_pixels, arr.ind = TRUE)[, 1]), 1)
})

test_that("all debug returns and image metadata remain available", {
  for (mode in c(
    "normals",
    "depth",
    "raw_depth",
    "position",
    "uv",
    "all",
    "none"
  )) {
    result = render_regression(debug = mode)
    expect_true(length(result) > 0)
  }
  a = render_regression(transparent_background = TRUE)
  expect_equal(dim(a), c(23L, 31L, 4L))
  expect_true(any(a[,, 4] == 0))
  expect_true(any(a[,, 4] == 1))
  expect_s3_class(a, "rayimg")
  expect_identical(attr(a, "channels"), c("Red", "Green", "Blue", "Alpha"))
  expect_identical(attr(a, "colorspace")$name, "sRGB")
  expect_true(attr(a, "source_linear"))
})

write_raster_texture = function(channels) {
  path = tempfile(fileext = ".tga")
  header = integer(18)
  header[3] = if (length(channels) <= 2L) 3L else 2L
  header[13] = header[15] = 1L
  header[17] = 8L * length(channels)
  header[18] = if (length(channels) %in% c(2L, 4L)) 8L else 0L
  if (length(channels) >= 3L) {
    channels[1:3] = channels[3:1]
  }
  writeBin(as.raw(c(header, channels)), path)
  path
}

test_that("gray and gray-alpha assets match RGB and RGBA rendering", {
  paths = vapply(
    list(128L, c(128L, 96L), rep(128L, 3), c(rep(128L, 3), 96L)),
    write_raster_texture,
    ""
  )
  on.exit(unlink(paths))
  outputs = lapply(paths, function(path) {
    render_regression(
      xy_rect_mesh(
        material = material_list(
          texture_location = path,
          type = "color",
          culling = "none"
        )
      ),
      debug = "all"
    )
  })
  expect_identical(outputs[[1]], outputs[[3]])
  expect_identical(outputs[[2]], outputs[[4]])
  # A refraction-only environment variant used to escape its deletion predicate.
  env = paths[1]
  for (i in 1:3) {
    expect_no_error(render_regression(
      sphere_mesh(material = material_list(ior = 1.5)),
      environment_map = env,
      background_sharpness = 0.1
    ))
  }
})

test_that("vertex and object-normal shaders initialize their position transform", {
  texture = write_raster_texture(c(128L, 128L, 255L))
  on.exit(unlink(texture))
  for (type in c("vertex", "diffuse", "phong")) {
    material = if (type == "vertex") {
      material_list(type = type)
    } else {
      material_list(type = type, normal_texture_location = texture)
    }
    scene = sphere_mesh(material = material)
    a = render_regression(scene, tangent_space_normals = FALSE, debug = "all")
    b = render_regression(scene, tangent_space_normals = FALSE, debug = "all")
    expect_identical(a, b)
    occupied = a$depth != 1
    expect_true(any(occupied))
    expect_true(all(a$positionz[occupied] < -2 & a$positionz[occupied] > -5))
  }
})

test_that("object-normal point lighting does not depend on previous fragments", {
  texture = write_raster_texture(c(128L, 128L, 255L))
  on.exit(unlink(texture))
  scene = sphere_mesh(
    material = material_list(normal_texture_location = texture)
  )
  args = list(
    scene = scene,
    width = 39,
    height = 25,
    fsaa = 1,
    plot = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    tangent_space_normals = FALSE,
    light_info = point_light(c(1, 1, 3)),
    debug = "all"
  )
  serial = do.call(rasterize_scene, c(args, list(parallel = FALSE)))
  withr::local_options(cores = 4L)
  expect_identical(do.call(rasterize_scene, args), serial)
})
