test_that("load_obj works", {
  testthat::skip_on_cran()

  obj_path = test_path("testdata", "packr.obj")
  # Binary/asset exclusions omit packr.obj from the source tarball. Keep an
  # actual OBJ + MTL parse/render test in packaged checks as well.
  if (!file.exists(obj_path)) {
    fixture = tempfile()
    dir.create(fixture)
    on.exit(unlink(fixture, recursive = TRUE))
    obj_path = file.path(fixture, "fixture.obj")
    writeLines(
      c("newmtl matte", "Kd 0.8 0.2 0.1", "d 1"),
      file.path(fixture, "fixture.mtl")
    )
    writeLines(
      c(
        "mtllib fixture.mtl",
        "v 3 3 0",
        "v 6 3 0",
        "v 4.5 6 0",
        "vt 0 0",
        "vt 1 0",
        "vt 0.5 1",
        "vn 0 0 1",
        "usemtl matte",
        "f 1/1/1 2/2/1 3/3/1"
      ),
      obj_path
    )
  }

  tile = obj_mesh(obj_path)
  table = sphere_mesh(
    c(0, 0, -1e3),
    radius = 1e3,
    material = material_list(diffuse = "grey40")
  )
  scene = add_shape(table, tile)
  testthat::expect_no_error(
    rasterize_scene(
      scene,
      lookat = c(4.5, 4, 0),
      lookfrom = c(4.5, -16, 20),
      light_info = directional_light(c(5, -7, 7), intensity = 2.5),
      fsaa = 1,
      plot = FALSE,
      parallel = FALSE
    )
  )
})
