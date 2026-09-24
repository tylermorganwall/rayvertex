# Usage: Rscript tools/compare-rasterizer-corpus.R BEFORE_RDS AFTER_RDS OUTPUT_CSV
args = commandArgs(TRUE)
rows = list()
compare_corpus = function(a, b, path) {
  if (is.list(a) && is.list(b) && identical(names(a), names(b))) {
    for (name in names(a)) {
      compare_corpus(a[[name]], b[[name]], paste(path, name, sep = "/"))
    }
  } else {
    numeric_pair = is.double(a) && is.double(b) && length(a) == length(b)
    finite = if (numeric_pair) is.finite(a) & is.finite(b) else logical()
    difference = if (numeric_pair) abs(a[finite] - b[finite]) else numeric()
    rows[[length(rows) + 1L]] <<- data.frame(
      path,
      exact = identical(a, b),
      attributes_exact = identical(attributes(a), attributes(b)),
      max_absolute = if (length(difference)) max(difference) else 0,
      changed_finite = sum(difference != 0),
      nonfinite_exact = if (numeric_pair) {
        identical(a[!finite], b[!finite])
      } else {
        NA
      }
    )
  }
}
compare_corpus(readRDS(args[1]), readRDS(args[2]), "")
write.csv(do.call(rbind, rows), args[3], row.names = FALSE)
