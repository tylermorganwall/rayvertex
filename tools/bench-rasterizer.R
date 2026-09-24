# Usage: Rscript tools/bench-rasterizer.R LIBRARY OUTPUT_DIR CASE WIDTH HEIGHT FSAA CORES REPS
# Run under /usr/bin/time -l (macOS) or -v (Linux) to record peak process RSS.
args = commandArgs(TRUE)
stopifnot(length(args) == 8L)
.libPaths(c(args[1], .libPaths()))
suppressPackageStartupMessages(library(rayvertex))
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
source("tools/rasterizer-fixtures.R")
out = args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
case = args[3]
w = as.integer(args[4])
h = as.integer(args[5])
fsaa = as.integer(args[6])
cores = as.integer(args[7])
reps = as.integer(args[8])
options(cores = cores, rayvertex.cores = cores)
fixture = rasterizer_fixture(case)
params = modifyList(
  list(
    width = w,
    height = h,
    fsaa = fsaa,
    plot = FALSE,
    parallel = cores > 1,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    fov = 40,
    shadow_map = FALSE,
    shadow_map_dims = c(256L, 256L)
  ),
  fixture
)
key = paste(case, w, h, fsaa, cores, sep = "-")
rows = vector("list", reps + 1L)
for (i in 0:reps) {
  gc()
  start = proc.time()[["elapsed"]]
  result = do.call(rasterize_scene, params)
  elapsed = (proc.time()[["elapsed"]] - start) * 1000
  rows[[i + 1L]] = data.frame(
    case,
    width = w,
    height = h,
    fsaa,
    cores,
    regime = if (i == 0L) "process_cold" else "warm",
    sample = i,
    elapsed_ms = elapsed
  )
}
saveRDS(result, file.path(out, paste0(key, "-image.rds")), compress = FALSE)
debug_params = modifyList(params, list(debug = "all"))
saveRDS(
  do.call(rasterize_scene, debug_params),
  file.path(out, paste0(key, "-buffers.rds")),
  compress = FALSE
)
write.csv(
  do.call(rbind, rows),
  file.path(out, paste0(key, "-times.csv")),
  row.names = FALSE
)
# Separate allocation run: profiler overhead is excluded from elapsed samples.
memfile = file.path(out, paste0(key, "-Rprofmem.txt"))
Rprofmem(memfile)
invisible(do.call(rasterize_scene, params))
Rprofmem(NULL)
alloc = suppressWarnings(as.numeric(sub(" .*", "", readLines(memfile))))
write.csv(
  data.frame(case, R_allocation_bytes = sum(alloc, na.rm = TRUE)),
  file.path(out, paste0(key, "-allocations.csv")),
  row.names = FALSE
)
capture.output(sessionInfo(), file = file.path(out, "session-info.txt"))
# Diagnostic runs are separate from the uninstrumented timing/allocation runs.
# Existing callbacks mark boundaries without changing the installed baseline.
bench_marks = list()
trace(
  "print_time",
  where = asNamespace("rayvertex"),
  print = FALSE,
  tracer = quote({
    .GlobalEnv$bench_marks[[length(.GlobalEnv$bench_marks) + 1L]] =
      c(label = message_text, seconds = proc.time()[["elapsed"]])
  })
)
trace(
  "rasterize",
  where = asNamespace("rayvertex"),
  print = FALSE,
  tracer = quote({
    .GlobalEnv$bench_native_args = mget(
      names(formals(rayvertex:::rasterize)),
      envir = environment()
    )
  })
)
invisible(do.call(rasterize_scene, params))
untrace("print_time", where = asNamespace("rayvertex"))
untrace("rasterize", where = asNamespace("rayvertex"))
marks = as.data.frame(do.call(rbind, bench_marks))
marks$seconds = as.numeric(marks$seconds)
marks$interval_ms = c(NA_real_, diff(marks$seconds) * 1000)
write.csv(
  marks,
  file.path(out, paste0(key, "-boundaries.csv")),
  row.names = FALSE
)
native_ms = numeric(reps)
profile_path = file.path(normalizePath(out), paste0(key, "-native-phases.csv"))
if (file.exists(profile_path)) {
  unlink(profile_path)
}
for (i in seq_len(reps)) {
  gc()
  start = proc.time()[["elapsed"]]
  invisible(do.call(rayvertex:::rasterize, bench_native_args))
  native_ms[i] = (proc.time()[["elapsed"]] - start) * 1000
}
write.csv(
  data.frame(sample = seq_len(reps), native_ms),
  file.path(out, paste0(key, "-native.csv")),
  row.names = FALSE
)
Sys.setenv(RAYVERTEX_PROFILE = profile_path)
for (i in seq_len(reps)) {
  invisible(do.call(rayvertex:::rasterize, bench_native_args))
}
Sys.unsetenv("RAYVERTEX_PROFILE")
cat(
  key,
  "median warm ms:",
  median(vapply(rows[-1], function(x) x$elapsed_ms, 0)),
  "\n"
)
