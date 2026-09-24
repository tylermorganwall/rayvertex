# Short repeat diagnostic, without debug serialization or allocation profiling.
# Usage: Rscript tools/bench-rasterizer-repeat.R LIB OUT_CSV CASE CORES REPS
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
source("tools/rasterizer-fixtures.R")
cores = as.integer(args[4])
reps = as.integer(args[5])
options(cores = cores)
params = modifyList(
  list(
    width = 800,
    height = 800,
    fsaa = 1,
    plot = FALSE,
    parallel = cores > 1,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    shadow_map = FALSE,
    shadow_map_dims = c(256L, 256L)
  ),
  rasterizer_fixture(args[3])
)
trace(
  "rasterize",
  where = asNamespace("rayvertex"),
  print = FALSE,
  tracer = quote({
    .GlobalEnv$repeat_native_args = mget(
      names(formals(rayvertex:::rasterize)),
      environment()
    )
  })
)
invisible(do.call(rasterize_scene, params))
untrace("rasterize", where = asNamespace("rayvertex"))
rows = list()
for (regime in c("warm", "native")) {
  for (i in seq_len(reps)) {
    gc()
    start = proc.time()[["elapsed"]]
    if (regime == "warm") {
      invisible(do.call(rasterize_scene, params))
    } else {
      invisible(do.call(rayvertex:::rasterize, repeat_native_args))
    }
    rows[[length(rows) + 1L]] = data.frame(
      case = args[3],
      cores,
      regime,
      sample = i,
      elapsed_ms = 1000 * (proc.time()[["elapsed"]] - start)
    )
  }
}
write.csv(do.call(rbind, rows), args[2], row.names = FALSE)
