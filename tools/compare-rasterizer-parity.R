# Usage: Rscript tools/compare-rasterizer-parity.R BEFORE_RDS AFTER_RDS OUTPUT_CSV
args = commandArgs(TRUE)
a = readRDS(args[1])
b = readRDS(args[2])
rows = list()
for (case in names(a)) {
  before = a[[case]]
  after = b[[case]]
  if (!is.list(before)) {
    before = list(image = before)
    after = list(image = after)
  }
  for (buffer in names(before)) {
    x = before[[buffer]]
    y = after[[buffer]]
    stopifnot(identical(dim(x), dim(y)))
    finite = is.finite(x) & is.finite(y)
    d = abs(x[finite] - y[finite])
    rows[[length(rows) + 1L]] = data.frame(
      case,
      buffer,
      exact = identical(x, y),
      max_absolute = if (length(d)) max(d) else 0,
      mean_absolute = if (length(d)) mean(d) else 0,
      changed_samples = sum(d != 0),
      nonfinite_mismatch = sum(
        is.finite(x) != is.finite(y) | is.na(x) != is.na(y)
      ),
      attributes_identical = identical(attributes(x), attributes(y))
    )
  }
}
write.csv(do.call(rbind, rows), args[3], row.names = FALSE)
