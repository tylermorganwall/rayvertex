# Usage: Rscript tools/test-rasterizer-package.R LIB OUTPUT_CSV
args = commandArgs(TRUE)
.libPaths(c(args[1], .libPaths()))
library(rayvertex)
stopifnot(
  normalizePath(find.package("rayvertex")) ==
    normalizePath(file.path(args[1], "rayvertex"))
)
Sys.setenv(NOT_CRAN = "true")
result = testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
rows = as.data.frame(result)
rows = rows[, !vapply(rows, is.list, logical(1))]
write.csv(rows, args[2], row.names = FALSE)
cat(
  "Assertions passed:",
  sum(rows$passed),
  "failed:",
  sum(rows$failed),
  "skipped:",
  sum(rows$skipped),
  "\n"
)
