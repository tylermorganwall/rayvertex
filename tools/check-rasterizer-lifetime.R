# POSIX developer diagnostic: high-water resident memory during 100 refraction-
# only renders, with explicit R GC and no retained images. Not a leak detector.
# Usage: Rscript tools/check-rasterizer-lifetime.R LIB OUTPUT_CSV
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
Rcpp::cppFunction(
  'double resident_high_water() {
  struct rusage usage;
  getrusage(RUSAGE_SELF, &usage);
  return static_cast<double>(usage.ru_maxrss);
}',
  includes = "#include <sys/resource.h>"
)
scale = if (Sys.info()[["sysname"]] == "Darwin") 1 else 1024
texture = tempfile(fileext = ".ppm")
writeBin(
  c(charToRaw("P6\n512 256\n255\n"), as.raw(rep(c(90, 160, 220), 512L * 256L))),
  texture
)
scene = sphere_mesh(
  material = material_list(ior = 1.5, reflection_intensity = 0)
)
gc()
rows = list(data.frame(
  render = 0,
  peak_rss_bytes = resident_high_water() * scale
))
for (i in 1:100) {
  invisible(rasterize_scene(
    scene,
    width = 64,
    height = 64,
    fsaa = 1,
    plot = FALSE,
    parallel = FALSE,
    lookfrom = c(0, 0, 4),
    lookat = c(0, 0, 0),
    shadow_map = FALSE,
    environment_map = texture
  ))
  gc()
  if (i %% 10 == 0) {
    rows[[length(rows) + 1L]] = data.frame(
      render = i,
      peak_rss_bytes = resident_high_water() * scale
    )
  }
}
unlink(texture)
write.csv(do.call(rbind, rows), args[2], row.names = FALSE)
