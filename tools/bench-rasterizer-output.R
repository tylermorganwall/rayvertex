# Usage: Rscript tools/bench-rasterizer-output.R LIB OUT WIDTH HEIGHT REPS
# Stage-only diagnostics, separate from full public-call benchmarks and RSS.
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
out = args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
width = as.integer(args[3])
height = as.integer(args[4])
reps = as.integer(args[5])
buffers = rasterize_scene(
  sphere_mesh(),
  width = width,
  height = height,
  fsaa = 1,
  plot = FALSE,
  parallel = FALSE,
  shadow_map = FALSE,
  lookfrom = c(0, 0, 4),
  lookat = c(0, 0, 0),
  debug = "all"
)
image = rayvertex:::raster_output_image(buffers)
stages = list(
  assemble_decode_orient = function() rayvertex:::raster_output_image(buffers),
  clamp = function() rayvertex:::clamp_raster_image(image)
)
rows = list()
allocations = list()
for (stage in names(stages)) {
  for (variant in c("reference", "fused")) {
    if (variant == "reference") {
      Sys.setenv(RAYVERTEX_REFERENCE_OUTPUT = "1")
    } else {
      Sys.unsetenv("RAYVERTEX_REFERENCE_OUTPUT")
    }
    invisible(stages[[stage]]())
    for (i in seq_len(reps)) {
      gc()
      start = proc.time()[["elapsed"]]
      result = stages[[stage]]()
      ms = 1000 * (proc.time()[["elapsed"]] - start)
      rows[[length(rows) + 1L]] = data.frame(stage, variant, sample = i, ms)
    }
    if (variant == "reference") {
      reference = result
    } else {
      stopifnot(identical(result, reference))
    }
    path = file.path(out, paste0(stage, "-", variant, "-Rprofmem.txt"))
    Rprofmem(path)
    invisible(stages[[stage]]())
    Rprofmem(NULL)
    sizes = suppressWarnings(as.numeric(sub(" .*", "", readLines(path))))
    allocations[[length(allocations) + 1L]] = data.frame(
      stage,
      variant,
      R_allocation_bytes = sum(sizes, na.rm = TRUE)
    )
  }
}
timings = do.call(rbind, rows)
write.csv(timings, file.path(out, "samples.csv"), row.names = FALSE)
write.csv(
  do.call(rbind, allocations),
  file.path(out, "allocations.csv"),
  row.names = FALSE
)
write.csv(
  aggregate(ms ~ stage + variant, timings, function(x) {
    c(median = median(x), p95 = unname(quantile(x, 0.95)))
  }),
  file.path(out, "summary.csv"),
  row.names = FALSE
)
writeLines(
  sprintf(
    "%s x %s, %s samples per stage/variant; all stage outputs exact",
    width,
    height,
    reps
  ),
  file.path(out, "settings.txt")
)
