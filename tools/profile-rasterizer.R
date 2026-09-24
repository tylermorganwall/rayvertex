# Usage: Rscript tools/profile-rasterizer.R LIB PID_FILE CASE
# Sample this process after PID_FILE appears; phase here is isolated native work.
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
source("tools/rasterizer-fixtures.R")
options(cores = 4L)
fixture = rasterizer_fixture(args[3])
trace(
  "rasterize",
  where = asNamespace("rayvertex"),
  print = FALSE,
  tracer = quote({
    .GlobalEnv$profile_native_args = mget(
      names(formals(rayvertex:::rasterize)),
      environment()
    )
  })
)
invisible(do.call(
  rasterize_scene,
  modifyList(
    list(
      width = 800,
      height = 800,
      fsaa = 1,
      plot = FALSE,
      shadow_map = FALSE,
      shadow_map_dims = c(256, 256),
      lookfrom = c(0, 0, 4),
      lookat = c(0, 0, 0),
      fov = 40
    ),
    fixture
  )
))
untrace("rasterize", where = asNamespace("rayvertex"))
writeLines(as.character(Sys.getpid()), args[2])
for (i in 1:100) {
  invisible(do.call(rayvertex:::rasterize, profile_native_args))
}
