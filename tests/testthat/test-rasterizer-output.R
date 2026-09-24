test_that("fused output matches scalar decoding, orientation, alpha and metadata", {
  withr::local_seed(12)
  withr::local_envvar(RAYVERTEX_REFERENCE_OUTPUT = NA)
  values = c(
    NA_real_,
    NaN,
    -Inf,
    Inf,
    -0,
    0,
    -1,
    1,
    10,
    0.04045,
    0.04045 - 1e-17,
    0.04045 + 1e-17,
    .Machine$double.xmin,
    .Machine$double.xmax,
    runif(10000, -0.1, 5)
  )
  for (dims in list(c(1, 1), c(1, 37), c(41, 1), c(39, 53))) {
    channels = lapply(seq_len(4), function(i) {
      matrix(rep(values, length.out = prod(dims)), dims[1], dims[2])
    })
    names(channels) = c("r", "g", "b", "a")
    snapshot = unserialize(serialize(channels, NULL))
    reference = withr::with_envvar(
      c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
      rayvertex:::raster_output_image(channels)
    )
    actual = rayvertex:::raster_output_image(channels)
    expect_identical(actual, reference)
    expect_identical(channels, snapshot)
    if (any(dims == 1)) {
      # Delegate singleton behavior to the installed dependency, including its
      # current dimension-dropping error or a future correction there.
      scalar = tryCatch(rayimage::render_clamp(reference), error = identity)
      fused = tryCatch(rayvertex:::clamp_raster_image(actual), error = identity)
      if (inherits(scalar, "error")) {
        expect_s3_class(fused, "error")
        expect_identical(conditionMessage(fused), conditionMessage(scalar))
      } else {
        expect_identical(fused, scalar)
      }
    } else {
      expect_identical(
        rayvertex:::clamp_raster_image(actual),
        rayimage::render_clamp(reference)
      )
    }
    expect_identical(actual, reference)
  }
  # Independent channel values make accidental plane/coordinate swaps visible.
  channels = setNames(
    lapply(1:4, function(i) {
      matrix(sample(values, 103 * 107, replace = TRUE), 103, 107)
    }),
    c("r", "g", "b", "a")
  )
  reference = withr::with_envvar(
    c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
    rayvertex:::raster_output_image(channels)
  )
  expect_identical(rayvertex:::raster_output_image(channels), reference)
  expect_error(
    rayvertex:::assemble_raster_output(
      matrix(0, 1, 2),
      matrix(0),
      matrix(0),
      matrix(0)
    ),
    "identical dimensions"
  )
  hdr = rayimage::ray_read_image(
    array(
      rep(c(NA_real_, NaN, -Inf, Inf, -0, -1, 2), length.out = 3 * 5 * 4),
      c(3, 5, 4)
    ),
    source_linear = TRUE,
    assume_colorspace = rayimage::CS_SRGB
  )
  attr(hdr, "exposure") = 3
  attr(hdr, "iso") = 400
  snapshot = unserialize(serialize(hdr, NULL))
  expect_identical(
    rayvertex:::clamp_raster_image(hdr),
    rayimage::render_clamp(hdr)
  )
  expect_identical(hdr, snapshot)
})

test_that("fused output preserves effects, prepared scenes and debug results", {
  withr::local_options(cores = 2L)
  withr::local_envvar(RAYVERTEX_REFERENCE_OUTPUT = NA)
  scene = add_shape(
    sphere_mesh(material = material_list(diffuse = c(2, 0.5, 0.1))),
    cube_mesh(
      position = c(0.5, 0, 0.3),
      material = material_list(dissolve = 0.4)
    )
  )
  prepared = prepare_scene(scene)
  for (tonemap in c("raw", "reinhard", "uncharted", "hbd")) {
    for (fsaa in c(1, 2)) {
      args = list(
        scene = scene,
        width = 43,
        height = 31,
        fsaa = fsaa,
        plot = FALSE,
        lookfrom = c(0, 0, 4),
        lookat = c(0, 0, 0),
        shadow_map = TRUE,
        shadow_map_dims = c(17, 13),
        ssao = TRUE,
        tonemap = tonemap,
        bloom = fsaa == 2,
        transparent_background = TRUE
      )
      reference = withr::with_envvar(
        c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
        do.call(rasterize_scene, args)
      )
      expect_identical(do.call(rasterize_scene, args), reference)
      args$scene = prepared
      expect_identical(do.call(rasterize_scene, args), reference)
    }
  }
  args$debug = "all"
  reference = withr::with_envvar(
    c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
    do.call(rasterize_scene, args)
  )
  expect_identical(do.call(rasterize_scene, args), reference)
})

test_that("fused output preserves encoded PNG bytes", {
  withr::local_envvar(RAYVERTEX_REFERENCE_OUTPUT = NA)
  paths = c(tempfile(fileext = ".png"), tempfile(fileext = ".png"))
  withr::defer(unlink(paths))
  args = list(
    scene = cube_mesh(),
    width = 47,
    height = 29,
    fsaa = 2,
    plot = FALSE,
    parallel = FALSE,
    shadow_map = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0)
  )
  reference = withr::with_envvar(
    c(RAYVERTEX_REFERENCE_OUTPUT = "1"),
    do.call(rasterize_scene, c(args, list(filename = paths[1])))
  )
  expect_identical(
    do.call(rasterize_scene, c(args, list(filename = paths[2]))),
    reference
  )
  expect_identical(
    readBin(paths[1], "raw", file.info(paths[1])$size),
    readBin(paths[2], "raw", file.info(paths[2])$size)
  )
})
