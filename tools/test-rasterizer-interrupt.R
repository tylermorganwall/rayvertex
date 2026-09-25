# A parent sends SIGINT only after the native-entry marker appears.
# Usage: Rscript tools/test-rasterizer-interrupt.R LIB MARKER RESULT MODE
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
source("tools/rasterizer-fixtures.R")
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
options(cores = 4L)
invisible(gc())
baseline = rayvertex:::prepared_scene_lifetime()
scene = rasterizer_grid(131072, 64, 0.1)
if (args[4] == "prepared") {
  scene = prepare_scene(scene)
}
small = list(
  scene = cube_mesh(),
  width = 31,
  height = 23,
  fsaa = 1,
  plot = FALSE,
  shadow_map = FALSE,
  lookfrom = c(0, 0, 4),
  lookat = c(0, 0, 0)
)
reference = do.call(rasterize_scene, small)
trace(
  "rasterize",
  where = asNamespace("rayvertex"),
  print = FALSE,
  tracer = quote(writeLines(as.character(Sys.getpid()), .GlobalEnv$args[2]))
)
interrupted = FALSE
tryCatch(
  {
    rasterize_scene(
      scene,
      width = 1500,
      height = 1000,
      fsaa = 1,
      plot = FALSE,
      shadow_map = FALSE,
      lookfrom = c(0, 0, 4),
      lookat = c(0, 0, 0)
    )
  },
  interrupt = function(e) {
    interrupted <<- TRUE
  },
  error = function(e) {
    if (!identical(conditionMessage(e), "C++ call interrupted by the user.")) {
      stop(e)
    }
    interrupted <<- TRUE
  }
)
untrace("rasterize", where = asNamespace("rayvertex"))
stopifnot(interrupted, identical(do.call(rasterize_scene, small), reference))
rm(scene)
invisible(gc())
stopifnot(identical(rayvertex:::prepared_scene_lifetime(), baseline))
writeLines(
  paste(
    args[4],
    "SIGINT caught; subsequent render exact; handles/assets released"
  ),
  args[3]
)
