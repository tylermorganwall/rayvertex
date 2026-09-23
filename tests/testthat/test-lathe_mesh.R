test_that("closed lathe meshes are welded, nondegenerate, and outward facing", {
  profile = rbind(c(0, 0), c(1, 0), c(1, 2), c(0, 2))
  mesh = lathe_mesh(profile, segments = 16)
  expect_s3_class(validate_mesh(mesh), "ray_mesh")
  vertices = mesh$vertices[[1]]
  faces = mesh$shapes[[1]]$indices + 1L
  expect_equal(nrow(vertices), 34)
  expect_equal(nrow(faces), 64)
  expect_equal(nrow(unique(vertices)), nrow(vertices))
  edges = rbind(faces[, c(1, 2)], faces[, c(2, 3)], faces[, c(3, 1)])
  keys = paste(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2]))
  expect_true(all(table(keys) == 2L))
  expect_true(all(tapply(sign(edges[, 2] - edges[, 1]), keys, sum) == 0))
  # Euler characteristic of a closed genus-zero surface.
  expect_equal(nrow(vertices) - length(unique(keys)) + nrow(faces), 2)
  a = vertices[faces[, 1], ]
  b = vertices[faces[, 2], ] - a
  c = vertices[faces[, 3], ] - a
  cross = cbind(
    b[, 2] * c[, 3] - b[, 3] * c[, 2],
    b[, 3] * c[, 1] - b[, 1] * c[, 3],
    b[, 1] * c[, 2] - b[, 2] * c[, 1]
  )
  expect_true(all(rowSums(cross^2) > 0))
  # Signed volume equals the area of a regular 16-gon times height.
  expect_equal(sum(a * cross) / 6, 16 * sin(2 * pi / 16))
  expect_equal(rowSums(mesh$normals[[1]]^2), rep(1, 34))
  expect_equal(as.numeric(mesh$normals[[1]][1, ]), c(0, -1, 0))
  expect_equal(as.numeric(mesh$normals[[1]][34, ]), c(0, 1, 0))
})

test_that("lathe normals follow the profile and reverse with its order", {
  # Horizontal caps, vertical outer/inner walls, and a rounded rim.
  profile = rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(1, 2),
    c(.95, 2.05),
    c(.9, 2),
    c(.9, 1),
    c(.9, .1),
    c(0, .1)
  )
  mesh = lathe_mesh(profile, segments = 8)
  normals = mesh$normals[[1]]
  expect_equal(as.numeric(normals[10, ]), c(1, 0, 0))
  expect_equal(as.numeric(normals[42, ]), c(-1, 0, 0))
  reversed = lathe_mesh(profile[nrow(profile):1, ], segments = 8)
  # Match normals by position, independent of ring order.
  key = function(x) apply(round(x, 10), 1, paste, collapse = ",")
  order = match(key(mesh$vertices[[1]]), key(reversed$vertices[[1]]))
  expect_equal(unname(reversed$normals[[1]][order, ]), unname(-normals))
  cone = lathe_mesh(rbind(c(0, 0), c(1, 1), c(0, 2)), segments = 3)
  expect_true(all(is.finite(cone$normals[[1]])))
  expect_equal(rowSums(cone$normals[[1]]^2), rep(1, 5))
})

test_that("open profiles have continuous UVs with a separate texture seam", {
  mesh = lathe_mesh(
    data.frame(radius = c(1, 1, 1), height = c(0, 1, 3)),
    segments = 8,
    smooth = FALSE
  )
  shape = mesh$shapes[[1]]
  expect_equal(nrow(mesh$vertices[[1]]), 24)
  expect_equal(nrow(shape$indices), 32)
  expect_equal(nrow(mesh$normals[[1]]), 0)
  expect_false(any(shape$has_vertex_normals))
  expect_true(all(shape$has_vertex_tex))
  uv = mesh$texcoords[[1]]
  expect_equal(range(uv), c(0, 1))
  expect_equal(unique(uv[, 2]), c(0, 1 / 3, 1))
  expect_true(all(shape$tex_indices >= 0 & shape$tex_indices < nrow(uv)))
  face_u = matrix(uv[shape$tex_indices + 1L, 1], ncol = 3)
  expect_true(all(apply(face_u, 1, function(x) diff(range(x))) <= 1 / 8))
  # The same geometric seam vertex has both U=0 and U=1 texture coordinates.
  expect_equal(
    sort(unique(uv[shape$tex_indices[shape$indices == 0] + 1L, 1])),
    c(0, 1)
  )
})

test_that("lathe meshes use standard material and transform conventions", {
  profile = rbind(c(1, 0), c(1, 2))
  material = material_list(diffuse = "red", type = "phong")
  mesh = lathe_mesh(profile, segments = 8, material = material)
  transformed = lathe_mesh(
    profile,
    segments = 8,
    material = material,
    scale = c(2, 3, 4),
    angle = c(10, 20, 30),
    pivot_point = c(.5, 1, 0),
    order_rotation = c(3, 1, 2),
    position = c(1, 2, 3)
  )
  expected = mesh |>
    scale_mesh(c(2, 3, 4)) |>
    rotate_mesh(
      c(10, 20, 30),
      pivot_point = c(.5, 1, 0),
      order_rotation = c(3, 1, 2)
    ) |>
    translate_mesh(c(1, 2, 3))
  expect_equal(transformed, expected)
  expect_equal(mesh$materials[[1]][[1]], material)
})

test_that("invalid lathe profiles fail with useful errors", {
  valid = rbind(c(1, 0), c(1, 1))
  for (profile in list(
    NULL,
    c(1, 2),
    matrix(1, 2, 3),
    matrix(1, 1, 2),
    cbind(c(1, Inf), c(0, 1)),
    cbind(c(1, NA), c(0, 1)),
    matrix("x", 2, 2)
  )) {
    expect_error(lathe_mesh(profile), "two-column matrix")
  }
  expect_error(lathe_mesh(rbind(c(-1, 0), c(1, 1))), "nonnegative")
  expect_error(lathe_mesh(rbind(c(0, 0), c(0, 1))), "endpoints")
  expect_error(lathe_mesh(rbind(c(1, 0), c(0, 1), c(1, 2))), "endpoints")
  expect_error(lathe_mesh(rbind(c(1, 0), c(1, 0))), "distinct")
  expect_error(
    lathe_mesh(rbind(c(1, 0), c(1, 1), c(1, 0))),
    "reverse direction"
  )
  for (segments in list(0, 2, 3.5, NA, Inf, "8", c(8, 9), 2^31, NULL)) {
    expect_error(lathe_mesh(valid, segments = segments), "`segments`")
  }
  for (smooth in list(NA, 1, c(TRUE, FALSE), NULL)) {
    expect_error(lathe_mesh(valid, smooth = smooth), "`smooth`")
  }
})

test_that("lathe meshes render and round-trip through OBJ", {
  mesh = lathe_mesh(rbind(c(0, 0), c(1, 0), c(1, 2), c(0, 2)), segments = 12)
  filename = tempfile(fileext = ".obj")
  on.exit(unlink(c(filename, sub("\\.obj$", ".mtl", filename))))
  write_scene_to_obj(mesh, filename)
  restored = obj_mesh(filename)
  expect_equal(
    unname(restored$vertices[[1]]),
    unname(mesh$vertices[[1]]),
    tolerance = 1e-5
  )
  expect_equal(
    unname(restored$shapes[[1]]$indices),
    unname(mesh$shapes[[1]]$indices)
  )
  expect_equal(
    unname(restored$normals[[1]]),
    unname(mesh$normals[[1]]),
    tolerance = 1e-5
  )
  expect_equal(
    unname(restored$texcoords[[1]]),
    unname(mesh$texcoords[[1]]),
    tolerance = 1e-5
  )
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
  expect_true(dim(image)[3] %in% c(3, 4))
  expect_true(all(is.finite(image)))
  expect_gt(diff(range(image[,, 1:3])), 0)
})
