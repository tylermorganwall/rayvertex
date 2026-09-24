# Warm public calls with automatic GC, plus final cleanup inside the loop timer.
# Usage: Rscript tools/bench-rasterizer-sustained.R LIB OUTPUT_PREFIX CASE REPS
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
source("tools/rasterizer-fixtures.R")
options(cores = 1L)
params = modifyList(
  list(
    width = 800,
    height = 800,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    shadow_map = FALSE,
    shadow_map_dims = c(256L, 256L)
  ),
  rasterizer_fixture(args[3])
)
if (Sys.getenv("RAYVERTEX_PREPARED") == "1") {
  params$scene = prepare_scene(params$scene)
}
invisible(do.call(rasterize_scene, params))
gc()
reps = as.integer(args[4])
frame_ms = numeric(reps)
start_loop = proc.time()[["elapsed"]]
for (i in seq_len(reps)) {
  start = proc.time()[["elapsed"]]
  result = do.call(rasterize_scene, params)
  frame_ms[i] = 1000 * (proc.time()[["elapsed"]] - start)
}
start_cleanup = proc.time()[["elapsed"]]
rm(result)
gc()
end = proc.time()[["elapsed"]]
write.csv(
  data.frame(sample = seq_len(reps), frame_ms),
  paste0(args[2], "-frames.csv"),
  row.names = FALSE
)
write.csv(
  data.frame(
    case = args[3],
    samples = reps,
    total_ms = 1000 * (end - start_loop),
    cleanup_ms = 1000 * (end - start_cleanup),
    mean_with_cleanup_ms = 1000 * (end - start_loop) / reps,
    fps_with_cleanup = reps / (end - start_loop)
  ),
  paste0(args[2], "-loop.csv"),
  row.names = FALSE
)
