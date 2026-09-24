# Usage: Rscript tools/plot-rasterizer-structural.R RUN OUTPUT_PNG
args = commandArgs(TRUE)
run = args[1]
before = read.csv(file.path(run, "before-summary", "summary.csv"))
after = read.csv(file.path(run, "after-summary", "summary.csv"))
cases = c(
  "small",
  "grid100k",
  "grid500k",
  "grid1m",
  "alpha4",
  "alpha16",
  "alpha64",
  "shadow",
  "ssao",
  "toon",
  "shared_textures",
  "environment",
  "environment_shared",
  "point_lights"
)
cases = cases[cases %in% before$case & cases %in% after$case]
loop_ms = function(variant) {
  vapply(
    cases,
    function(case) {
      read.csv(file.path(
        run,
        variant,
        paste0(case, "-sustained-loop.csv")
      ))$mean_with_cleanup_ms
    },
    0
  )
}
times = rbind(loop_ms("before"), loop_ms("after"))
rss = rbind(
  before$single_render_peak_rss_bytes[match(cases, before$case)],
  after$single_render_peak_rss_bytes[match(cases, after$case)]
) /
  1024^2
colors = c("#6d7c89", "#087f8c")
png(args[2], width = 1900, height = 1100, res = 150, type = "cairo")
par(mfrow = c(1, 2), mar = c(5, 10, 4, 1), oma = c(2, 0, 2, 0))
barplot(
  times[, rev(seq_along(cases))],
  beside = TRUE,
  horiz = TRUE,
  names.arg = rev(cases),
  las = 1,
  cex.names = 0.7,
  col = colors,
  border = NA,
  xlab = "Sustained public mean (ms)",
  main = "Five calls plus output cleanup"
)
legend(
  "bottomright",
  c("Clipped reference e512c02", "Final d48a259"),
  fill = colors,
  bty = "n",
  cex = 0.7
)
barplot(
  rss[, rev(seq_along(cases))],
  beside = TRUE,
  horiz = TRUE,
  names.arg = rev(cases),
  las = 1,
  cex.names = 0.7,
  col = colors,
  border = NA,
  xlab = "Fresh-render peak RSS (MiB)",
  main = "Separate one-render processes"
)
mtext(
  "800 × 800 · FSAA 1 · one worker · ordinary API · unchanged quality",
  outer = TRUE,
  side = 3
)
mtext(
  "RSS includes R and fixture construction. Raw samples, allocation totals, phase timings and differences accompany the tables.",
  outer = TRUE,
  side = 1,
  cex = 0.7
)
dev.off()
