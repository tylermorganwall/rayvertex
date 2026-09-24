# Usage: Rscript tools/compare-rasterizer.R BEFORE AFTER OUTPUT_CSV
args = commandArgs(TRUE)
stopifnot(length(args) == 3L)
files = list.files(
  args[1],
  pattern = "-(image|buffers)\\.rds$",
  full.names = FALSE
)
results = list()
for (file in files) {
  other = file.path(args[2], file)
  if (!file.exists(other)) {
    next
  }
  before = readRDS(file.path(args[1], file))
  after = readRDS(other)
  if (!is.list(before)) {
    before = list(image = before)
    after = list(image = after)
  }
  for (name in names(before)) {
    a = before[[name]]
    b = after[[name]]
    stopifnot(identical(dim(a), dim(b)))
    delta = abs(as.numeric(a) - as.numeric(b))
    nonfinite_mismatch = sum(
      is.na(a) != is.na(b) | is.infinite(a) != is.infinite(b)
    )
    delta[is.na(delta) & (a == b | (is.na(a) & is.na(b)))] = 0
    where = if (all(is.na(delta))) {
      NA_integer_
    } else {
      which.max(replace(delta, is.na(delta), Inf))
    }
    results[[length(results) + 1L]] = data.frame(
      file,
      buffer = name,
      exact = identical(a, b),
      max_absolute = max(delta, na.rm = TRUE),
      mean_absolute = mean(delta, na.rm = TRUE),
      nonfinite_mismatch,
      changed_samples = sum(delta != 0, na.rm = TRUE),
      worst_linear_index = where,
      attributes_identical = identical(attributes(a), attributes(b))
    )
  }
}
write.csv(do.call(rbind, results), args[3], row.names = FALSE)
