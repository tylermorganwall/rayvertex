test_that("load_obj works", {
  obj_path = test_path("testdata", "packr.obj")
  tile <- obj_mesh(obj_path)
  table <- sphere_mesh(c(0, 0, -1e3), radius=1e3, material = material_list(diffuse="grey40"))
  scene <- add_shape(table, tile)
  testthat::expect_no_error(
    rasterize_scene(scene, lookat = c(4.5, 4, 0), lookfrom=c(4.5, -16, 20),
                    light_info = directional_light(c(5, -7, 7), intensity = 2.5))
  )
})
