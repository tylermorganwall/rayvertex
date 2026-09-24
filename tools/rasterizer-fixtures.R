# Developer-only deterministic fixtures. No random assets or downloads.
rasterizer_grid = function(
  triangles = 100000L,
  layers = 1L,
  alpha = 1,
  type = "diffuse"
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
    alpha4 = list(scene = rasterizer_grid(512L, 4L, 0.3)),
    alpha16 = list(scene = rasterizer_grid(512L, 16L, 0.15)),
    alpha64 = list(scene = rasterizer_grid(512L, 64L, 0.05)),
    ssao = list(scene = sphere_mesh(), ssao = TRUE),
    shadow = list(scene = sphere_mesh(), shadow_map = TRUE),
    toon = list(scene = sphere_mesh(material = material_list(type = "toon"))),
    shared_textures = rasterizer_shared_textures(),
    stop("Unknown fixture: ", name)
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
