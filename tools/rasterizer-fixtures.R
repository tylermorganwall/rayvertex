# Developer-only deterministic fixtures. No random assets or downloads.
rasterizer_grid = function(
  triangles = 100000L,
  layers = 1L,
  alpha = 1,
  type = "diffuse",
  reverse = FALSE
) {
  side = max(1L, as.integer(ceiling(sqrt(triangles / (2 * layers)))))
  xy = expand.grid(
    x = seq(-1, 1, length.out = side + 1L),
    y = seq(-1, 1, length.out = side + 1L)
  )
  vertices = cbind(xy$x, xy$y, 0.05 * sin(9 * xy$x) * cos(9 * xy$y))
  cells = expand.grid(x = 0:(side - 1L), y = 0:(side - 1L))
  a = as.integer(cells$x + (side + 1L) * cells$y)
  indices = rbind(
    cbind(a, a + 1L, a + side + 1L),
    cbind(a + 1L, a + side + 2L, a + side + 1L)
  )
  nv = nrow(vertices)
  vertices = do.call(
    rbind,
    lapply(seq_len(layers), function(i) {
      v = vertices
      v[, 3] = v[, 3] - (i - 1L) * 0.02
      v
    })
  )
  indices = do.call(
    rbind,
    lapply(seq_len(layers), function(i) {
      indices + (i - 1L) * nv
    })
  )
  if (reverse) {
    indices = indices[rev(seq_len(nrow(indices))), , drop = FALSE]
  }
  construct_mesh(
    vertices,
    indices,
    material = material_list(
      diffuse = c(0.7, 0.35, 0.15),
      dissolve = alpha,
      culling = "none",
      type = type
    )
  )
}

rasterizer_fixture = function(name) {
  switch(
    name,
    small = list(scene = cube_mesh()),
    grid100k = list(scene = rasterizer_grid(100000L)),
    grid500k = list(scene = rasterizer_grid(500000L)),
    grid1m = list(scene = rasterizer_grid(1000000L)),
    occluded = list(scene = rasterizer_grid(100000L, 16L, type = "phong")),
    overdraw = list(
      scene = rasterizer_grid(100000L, 16L, type = "phong", reverse = TRUE)
    ),
    alpha4 = list(scene = rasterizer_grid(512L, 4L, 0.3)),
    alpha16 = list(scene = rasterizer_grid(512L, 16L, 0.15)),
    alpha64 = list(scene = rasterizer_grid(512L, 64L, 0.05)),
    alpha129 = list(scene = rasterizer_grid(512L, 129L, 0.025)),
    slivers = rasterizer_slivers(),
    orthographic = list(
      scene = sphere_mesh(),
      fov = 0,
      ortho_dimensions = c(3, 2)
    ),
    near_crossing = list(
      scene = construct_mesh(
        rbind(c(-0.2, -0.2, 3.95), c(0.4, -0.2, 3), c(0, 0.4, 3)),
        matrix(0:2, 1),
        material = material_list(culling = "none")
      )
    ),
    ssao = list(scene = sphere_mesh(), ssao = TRUE),
    shadow = list(scene = sphere_mesh(), shadow_map = TRUE),
    toon = list(scene = sphere_mesh(material = material_list(type = "toon"))),
    shared_textures = rasterizer_shared_textures(),
    environment = rasterizer_environment(),
    environment_shared = rasterizer_environment_shared(),
    point_lights = list(
      scene = sphere_mesh(material = material_list(type = "phong")),
      light_info = do.call(
        rbind,
        lapply(1:8, function(i) {
          point_light(
            position = c(2 * cos(i), 2 * sin(i), 3),
            falloff = 0.2,
            falloff_quad = 0.05,
            intensity = 0.3
          )
        })
      )
    ),
    stop("Unknown fixture: ", name)
  )
}

rasterizer_slivers = function() {
  vertices = do.call(
    rbind,
    lapply(seq_len(64), function(i) {
      offset = (i - 32) / 64
      rbind(
        c(-1, -1 + offset, 0),
        c(1, 1 + offset, 0),
        c(1, 1 + offset + 0.006, 0)
      )
    })
  )
  list(
    scene = construct_mesh(
      vertices,
      matrix(seq_len(nrow(vertices)) - 1L, ncol = 3, byrow = TRUE),
      material = material_list(culling = "none")
    )
  )
}

rasterizer_shared_textures = function() {
  # The asset is generated before rendering, so this is OS-file-cache warm.
  texture = tempfile(fileext = ".ppm")
  writeBin(
    c(
      charToRaw("P6\n512 512\n255\n"),
      as.raw(rep(c(90, 160, 220), 512L * 512L))
    ),
    texture
  )
  scene = NULL
  for (i in 0:15) {
    object = cube_mesh(
      position = c((i %% 4 - 1.5) * 0.55, (i %/% 4 - 1.5) * 0.55, 0),
      scale = 0.4,
      material = material_list(
        texture_location = texture,
        diffuse_intensity = 0.5 + i / 32
      )
    )
    scene = add_shape(scene, object)
  }
  list(scene = scene, shadow_map = TRUE)
}

rasterizer_environment = function() {
  texture = tempfile(fileext = ".ppm")
  writeBin(
    c(
      charToRaw("P6\n512 256\n255\n"),
      as.raw(rep(c(90, 160, 220), 512L * 256L))
    ),
    texture
  )
  list(
    scene = sphere_mesh(
      material = material_list(
        reflection_intensity = 0.4,
        reflection_sharpness = 0.5
      )
    ),
    environment_map = texture,
    background_sharpness = 0.75
  )
}


rasterizer_environment_shared = function() {
  texture = tempfile(fileext = ".ppm")
  xy = expand.grid(x = 0:1023, y = 0:511)
  pixels = as.raw(as.vector(t(cbind(
    xy$x %% 256,
    xy$y %% 256,
    (xy$x + xy$y) %% 256
  ))))
  writeBin(c(charToRaw("P6\n1024 512\n255\n"), pixels), texture)
  scene = NULL
  for (i in 0:15) {
    scene = add_shape(
      scene,
      cube_mesh(
        position = c((i %% 4 - 1.5) * 0.55, (i %/% 4 - 1.5) * 0.55, 0),
        scale = 0.4,
        material = material_list(
          reflection_intensity = 0.2 + i / 32,
          reflection_sharpness = if (i %% 4 == 0) {
            1
          } else if (i %% 2 == 0) {
            0.5001
          } else {
            0.5
          },
          ior = if (i %% 3 == 0) 1.5 else 1
        )
      )
    )
  }
  list(scene = scene, environment_map = texture, background_sharpness = 0.5)
}
