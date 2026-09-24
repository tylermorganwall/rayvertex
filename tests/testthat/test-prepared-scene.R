prepared_render = function(scene, ...) {
  rasterize_scene(
    scene,
    width = 43,
    height = 31,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map_dims = c(29, 23),
    ...
  )
}

test_that("prepared geometry snapshots preserve outputs and original-list independence", {
  scene = sphere_mesh(material = material_list(type = "toon"))
  prepared = prepare_scene(scene)
  expect_s3_class(prepared, "rayvertex_prepared_scene")
  expect_output(print(prepared), "prepared scene")
  for (mode in c("none", "all", "normals", "position", "uv", "raw_depth")) {
    expect_identical(
      prepared_render(prepared, debug = mode),
      prepared_render(scene, debug = mode)
    )
  }
  expected = prepared_render(prepared)
  scene$vertices[[1]][, 1] = scene$vertices[[1]][, 1] + 10
  expect_identical(prepared_render(prepared), expected)
  updated = update_prepared_scene(prepared, scene)
  expect_identical(prepared_render(updated), prepared_render(scene))
  expect_false(identical(prepared_render(updated), expected))
  expect_identical(prepared_render(prepared), expected)
})

test_that("prepared snapshots evaluate frame options and callbacks afresh", {
  scene = sphere_mesh(material = material_list(type = "toon"))
  prepared = prepare_scene(scene)
  args = list(
    width = 41,
    height = 29,
    fsaa = 2,
    plot = FALSE,
    lookat = c(0, 0, 0),
    lookfrom = c(0, 1, 4),
    shadow_map_dims = c(31, 27),
    light_info = add_light(
      directional_light(),
      directional_light(direction = c(-1, 1, 0))
    ),
    debug = "all"
  )
  calls = 0L
  args$vertex_transform = function(x) {
    calls <<- calls + 1L
    x * c(1, 1, 0.9)
  }
  reference = do.call(rasterize_scene, c(list(scene = scene), args))
  expected_calls = calls
  for (i in 1:2) {
    calls = 0L
    expect_identical(
      do.call(rasterize_scene, c(list(scene = prepared), args)),
      reference
    )
    expect_equal(calls, expected_calls)
  }
  args$lookfrom = c(1, 2, 5)
  args$ssao = TRUE
  expect_identical(
    do.call(rasterize_scene, c(list(scene = prepared), args)),
    do.call(rasterize_scene, c(list(scene = scene), args))
  )
})

test_that("prepared material assets survive source changes and rebuild explicitly", {
  path = tempfile(fileext = ".ppm")
  on.exit(unlink(path))
  write_pixel = function(rgb) {
    writeBin(c(charToRaw("P6\n1 1\n255\n"), as.raw(rgb)), path)
  }
  write_pixel(c(255, 0, 0))
  scene = cube_mesh(
    material = material_list(type = "color", texture_location = path)
  )
  prepared = prepare_scene(scene)
  reference = prepared_render(scene)
  expect_identical(prepared_render(prepared), reference)
  info = rayvertex:::prepared_scene_info(prepared)
  expect_equal(info$texture_decodes, 1)
  write_pixel(c(0, 0, 255))
  expect_identical(prepared_render(prepared), reference)
  updated = update_prepared_scene(prepared, scene)
  expect_identical(prepared_render(updated), prepared_render(scene))
  expect_false(identical(prepared_render(updated), reference))
  unlink(path)
  expect_identical(prepared_render(prepared), reference)
})

test_that("prepared handles reject serialization and release owned assets", {
  gc()
  baseline = rayvertex:::prepared_scene_lifetime()
  handle = prepare_scene(cube_mesh())
  expect_equal(
    rayvertex:::prepared_scene_lifetime()$handles,
    baseline$handles + 1
  )
  invalid = unserialize(serialize(handle, NULL))
  expect_error(prepared_render(invalid), "Invalid or expired prepared scene")
  expect_error(
    update_prepared_scene(invalid, cube_mesh()),
    "Invalid or expired"
  )
  handle = NULL
  gc()
  expect_identical(rayvertex:::prepared_scene_lifetime(), baseline)
  for (i in 1:10) {
    handle = prepare_scene(cube_mesh())
    handle = NULL
    gc()
  }
  expect_identical(rayvertex:::prepared_scene_lifetime(), baseline)
})

test_that("prepared handles reject use after a process fork", {
  skip_on_os("windows")
  skip_on_cran()
  handle = prepare_scene(cube_mesh())
  job = parallel::mcparallel(
    tryCatch(rayvertex:::prepared_scene_info(handle), error = conditionMessage),
    silent = TRUE
  )
  result = parallel::mccollect(job)[[1]]
  expect_match(result, "different process")
})

test_that("prepared texture owners release on collection and partial failure", {
  gc()
  baseline = rayvertex:::prepared_scene_lifetime()
  path = tempfile(fileext = ".ppm")
  bad = tempfile(fileext = ".png")
  on.exit(unlink(c(path, bad)))
  writeBin(c(charToRaw("P6\n1 1\n255\n"), as.raw(c(32, 64, 128))), path)
  writeLines("not an image", bad)
  scene = cube_mesh(material = material_list(texture_location = path))
  for (i in 1:10) {
    handle = prepare_scene(scene)
    expect_gt(
      rayvertex:::prepared_scene_lifetime()$texture_bytes,
      baseline$texture_bytes
    )
    prepared_render(handle)
    handle = NULL
    gc()
    expect_identical(rayvertex:::prepared_scene_lifetime(), baseline)
  }
  broken = add_shape(
    scene,
    cube_mesh(material = material_list(texture_location = bad))
  )
  expect_error(prepare_scene(broken), "texture loading failed")
  gc()
  expect_identical(rayvertex:::prepared_scene_lifetime(), baseline)
})
