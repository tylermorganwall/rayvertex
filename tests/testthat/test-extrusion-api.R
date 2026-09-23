test_that("public sweep meshes preserve cap materials and optional parts", {
  path = rbind(c(0, 0, 0), c(0, 0, 2))
  surface = material_list(diffuse = "steelblue")
  cap = material_list(diffuse = "red")
  args = list(
    points = path,
    breaks = 5,
    smooth_normals = TRUE,
    material = surface,
    material_caps = cap
  )
  mesh = do.call(extruded_path_mesh, args)
  parts = do.call(extruded_path_mesh, c(args, list(separate_caps = TRUE)))
  expect_s3_class(validate_mesh(mesh), "ray_mesh")
  expect_named(parts, c("surface", "caps"))
  expect_length(parts$surface$shapes, 1)
  expect_length(parts$caps$shapes, 2)
  expect_identical(add_shape(parts$surface, parts$caps), mesh)
  expect_equal(mesh$materials[[1]][[1]], surface)
  expect_equal(mesh$materials[[2]][[1]], cap)
  expect_equal(mesh$materials[[3]][[1]], cap)
  for (i in seq_along(mesh$shapes)) {
    shape = mesh$shapes[[i]]
    expect_true(all(
      shape$indices >= 0 & shape$indices < nrow(mesh$vertices[[i]])
    ))
    expect_true(all(
      shape$norm_indices >= 0 & shape$norm_indices < nrow(mesh$normals[[i]])
    ))
    expect_true(all(
      shape$tex_indices >= 0 & shape$tex_indices < nrow(mesh$texcoords[[i]])
    ))
  }
  smooth = mesh$shapes[[1]]
  u = matrix(mesh$texcoords[[1]][smooth$tex_indices + 1L, 1], ncol = 3)
  expect_true(all(apply(u, 1, function(x) diff(range(x))) <= 1 / 30 + 1e-12))
  expect_null(extruded_path_mesh(path, u_min = .3, u_max = .3))
  expect_null(extruded_path_mesh(path, width = 0))
  expect_identical(
    extruded_path_mesh(path, width = 0, separate_caps = TRUE),
    list(surface = NULL, caps = NULL)
  )
  open = extruded_path_mesh(
    path,
    end_caps = c(FALSE, FALSE),
    separate_caps = TRUE
  )
  expect_null(open$caps)
})

test_that("extrusions use rayvertex transform and material conventions", {
  polygon = rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1))
  path = rbind(c(0, 0, 0), c(0, 1, 1), c(1, 2, 1))
  constructors = list(
    function(...) {
      extruded_path_mesh(path, breaks = 8, smooth_normals = TRUE, ...)
    },
    function(...) extruded_polygon_mesh(polygon, ...)
  )
  for (build in constructors) {
    material = material_list(diffuse = "orange", type = "phong")
    base = build(material = material)
    actual = build(
      material = material,
      scale = c(2, 3, 4),
      angle = c(10, 20, 30),
      pivot_point = c(.5, 1, 0),
      order_rotation = c(3, 1, 2),
      position = c(4, 5, 6)
    )
    expected = base |>
      scale_mesh(c(2, 3, 4)) |>
      rotate_mesh(
        c(10, 20, 30),
        pivot_point = c(.5, 1, 0),
        order_rotation = c(3, 1, 2)
      ) |>
      translate_mesh(c(4, 5, 6))
    expect_equal(actual, expected)
    expect_equal(actual$materials[[1]][[1]], material)
    expect_s3_class(validate_mesh(actual), "ray_mesh")
    expect_error(build(position = c(1, NA, 3)), "position")
    expect_error(build(angle = c(0, 1)), "angle")
    expect_error(build(pivot_point = Inf), "pivot_point")
    expect_error(build(order_rotation = c(1, 1, 3)), "order_rotation")
    expect_error(build(scale = 0), "scale")
  }
  expect_error(extruded_path_mesh(path, separate_caps = NA), "separate_caps")
})

test_that("extrusions rasterize and export without rayrender", {
  polygon = rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, 1))
  meshes = list(
    extruded_path_mesh(
      rbind(c(0, 0, 0), c(0, 1, 1), c(1, 2, 1)),
      breaks = 8,
      smooth_normals = TRUE
    ),
    extruded_polygon_mesh(rbind(polygon, polygon * .3), holes = 5)
  )
  for (mesh in meshes) {
    image = rasterize_scene(
      mesh,
      width = 32,
      height = 32,
      fsaa = 1,
      lookfrom = c(4, 3, 5),
      lookat = c(0, 1, 0),
      plot = FALSE,
      parallel = FALSE
    )
    expect_equal(dim(image)[1:2], c(32, 32))
    expect_true(all(is.finite(image)))
    expect_gt(diff(range(image)), 0)
    filename = tempfile(fileext = ".obj")
    write_scene_to_obj(mesh, filename)
    restored = obj_mesh(filename)
    expect_equal(
      sum(vapply(restored$shapes, function(s) nrow(s$indices), 0L)),
      sum(vapply(mesh$shapes, function(s) nrow(s$indices), 0L))
    )
    expect_equal(
      unname(get_mesh_bbox(restored)),
      unname(get_mesh_bbox(mesh)),
      tolerance = 1e-5
    )
    unlink(c(filename, sub("\\.obj$", ".mtl", filename)))
  }
})
