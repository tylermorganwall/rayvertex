# Same fixture/camera/quality as bench-rasterizer.R; one render in a fresh R
# process, with no retained debug copies, timing repetitions or Rprofmem pass.
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
source("tools/rasterizer-fixtures.R")
options(cores = as.integer(args[7]))
params = modifyList(
  list(
    width = as.integer(args[4]),
    height = as.integer(args[5]),
    fsaa = as.integer(args[6]),
    plot = FALSE,
    parallel = as.integer(args[7]) > 1,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    shadow_map = FALSE,
    shadow_map_dims = c(256L, 256L)
  ),
  rasterizer_fixture(args[3])
)
result = do.call(rasterize_scene, params)
stopifnot(all(dim(result) == c(as.integer(args[5]), as.integer(args[4]), 4L)))
