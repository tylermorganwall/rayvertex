test_that("displacement_sphere supports cube-based sphere displacement", {
  texture_dim = 8
  u = seq(0, 2 * pi, length.out = texture_dim)
  v = seq(0, 2 * pi, length.out = texture_dim)
  knit_texture = outer(u, v, function(x, y) {
    sin(4 * x + 0.75 * sin(3 * y))^2 + 0.35 * cos(6 * y)^2
  })
  knit_texture = (knit_texture - min(knit_texture)) / diff(range(knit_texture))
  knit_texture = array(
    rep(knit_texture, 3),
    dim = c(texture_dim, texture_dim, 3)
  )

  mesh = displacement_sphere(
    knit_texture,
    use_cube = TRUE,
    displacement_scale = 0.02,
    verbose = FALSE
  )

  testthat::expect_s3_class(mesh, "ray_mesh")
  testthat::expect_length(mesh$shapes, 1)
  testthat::expect_equal(ncol(mesh$vertices[[1]]), 3)
})
