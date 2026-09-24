# Illustrate an intentional correctness change, with an absolute-error heatmap.
# Usage: Rscript tools/plot-rasterizer-correction.R BEFORE_RDS AFTER_RDS OUTPUT_PNG
args = commandArgs(TRUE)
a = readRDS(args[1])
b = readRDS(args[2])
stopifnot(identical(dim(a), dim(b)), length(dim(a)) == 3L)
delta = apply(abs(a[,, 1:3] - b[,, 1:3]), c(1, 2), max)
limit = max(delta)
palette = hcl.colors(256, "Inferno")
heat = matrix(
  palette[1L + floor(255 * delta / max(limit, .Machine$double.eps))],
  nrow(delta),
  ncol(delta)
)
png(args[3], width = 1500, height = 570, res = 120)
par(mfrow = c(1, 3), mar = c(1, 1, 3, 1), oma = c(3, 0, 0, 0))
for (i in 1:3) {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  panel = if (i == 1L) {
    a
  } else if (i == 2L) {
    b
  } else {
    heat
  }
  if (i < 3L) {
    # Base graphics consumes the returned sRGB samples, without changing data.
    panel = array(as.numeric(panel), dim(panel))
  }
  rasterImage(as.raster(panel), 0, 0, 1, 1, interpolate = FALSE)
  title(c(
    "Original SSAO",
    "Corrected scalar SSAO",
    "Maximum channel absolute error"
  )[i])
}
mtext(
  sprintf(
    "Intentional correctness change. Error heatmap: dark = 0, bright = %.8f. Equivalent optimization phases have zero differences.",
    limit
  ),
  side = 1,
  outer = TRUE,
  line = 1
)
dev.off()
