test_that("rescale handles finite and non-finite values", {
  testthat::skip_on_cran()

  testthat::expect_equal(rayvertex:::rescale(c(1, 2, 3)), c(0, 0.5, 1))
  testthat::expect_equal(rayvertex:::rescale(c(5, 5), to = c(-1, 1)), c(0, 0))
  testthat::expect_equal(
    rayvertex:::rescale(c(NA, -Inf, 2, 4, Inf)),
    c(NA, -Inf, 0, 1, Inf)
  )
  testthat::expect_equal(
    rayvertex:::rescale(c(NA, -Inf, Inf)),
    c(NA, -Inf, Inf)
  )
})

test_that("rasterize_scene position debug path works", {
  testthat::skip_on_cran()

  image = rasterize_scene(
    cube_mesh(),
    width = 32,
    height = 32,
    fsaa = 1,
    debug = "position",
    plot = FALSE,
    parallel = FALSE
  )

  testthat::expect_equal(dim(image), c(32, 32, 3))
})

test_that("mesh3d_mesh centers the supplied mesh", {
  testthat::skip_on_cran()

  mesh = structure(
    list(
      vb = rbind(
        c(10, 12, 10),
        c(20, 20, 22),
        c(30, 30, 34),
        c(1, 1, 1)
      ),
      it = matrix(c(1L, 2L, 3L), nrow = 3)
    ),
    class = "mesh3d"
  )

  centered = mesh3d_mesh(mesh, center = TRUE)

  testthat::expect_equal(
    unname(get_mesh_center(centered)),
    c(0, 0, 0),
    tolerance = 1e-8
  )
})

test_that("text3d_mesh accepts color as a deprecated alias", {
  testthat::skip_on_cran()

  testthat::expect_no_error(text3d_mesh("x", color = "red"))
  testthat::expect_no_error(text3d_mesh(
    "x",
    font_color = "blue",
    color = "red"
  ))
})
