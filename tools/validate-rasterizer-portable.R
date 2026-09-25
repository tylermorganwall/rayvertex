# Called by .github/workflows/rasterizer-validation.yaml from the repository.
# Build both implementations on each platform; never loosen a tolerance to pass.
out = normalizePath(".")
out = file.path(out, "validation-results")
dir.create(out, showWarnings = FALSE)
root = Sys.getenv("RUNNER_TEMP", tempdir())
libraries = file.path(root, c("scalar-library", "accepted-library"))
for (lib in libraries) {
  dir.create(lib, showWarnings = FALSE)
}
statuses = list()
run_check = function(name, executable, arguments, env = character()) {
  log = file.path(out, paste0(name, ".log"))
  status = system2(
    executable,
    shQuote(arguments),
    stdout = log,
    stderr = log,
    env = env
  )
  statuses[[length(statuses) + 1L]] <<- data.frame(
    check = name,
    exit_code = status
  )
  write.csv(
    do.call(rbind, statuses),
    file.path(out, "status.csv"),
    row.names = FALSE
  )
  cat(name, ":", status, "\n")
  status == 0L
}
r = file.path(R.home("bin"), "R")
rscript = file.path(R.home("bin"), "Rscript")
writeLines(
  c(capture.output(sessionInfo()), system("git rev-parse HEAD", intern = TRUE)),
  file.path(out, "environment.txt")
)
for (i in 1:2) {
  source_path = if (i == 1L) file.path(root, "scalar-source") else "."
  if (
    !run_check(
      paste0("install-", i),
      r,
      c(
        "CMD",
        "INSTALL",
        "--preclean",
        paste0("--library=", libraries[i]),
        source_path
      )
    )
  ) {
    stop("Installation failed; see validation-results")
  }
}
run_check(
  "package-tests",
  rscript,
  c(
    "tools/test-rasterizer-package.R",
    libraries[2],
    file.path(out, "tests.csv")
  )
)
for (corpus in c("parity", "shader-parity")) {
  script = paste0("tools/rasterizer-", corpus, ".R")
  reference = file.path(out, paste0(corpus, "-scalar.rds"))
  run_check(
    paste0(corpus, "-scalar"),
    rscript,
    c(script, libraries[1], reference)
  )
  for (variant in c("default", "combined")) {
    result = file.path(out, paste0(corpus, "-", variant, ".rds"))
    switches = if (variant == "combined") {
      paste0(
        c(
          "RAYVERTEX_NORMAL_CACHE",
          "RAYVERTEX_BLOCK_COVERAGE",
          "RAYVERTEX_PARALLEL_BINS",
          "RAYVERTEX_VISIBILITY",
          "RAYVERTEX_PARITY_PREPARED"
        ),
        "=1"
      )
    } else {
      character()
    }
    if (variant == "combined") {
      switches = c(switches, "RAYVERTEX_MACROTILE_EDGE=32")
    }
    run_check(
      paste(corpus, variant, sep = "-"),
      rscript,
      c(script, libraries[2], result, reference),
      env = switches
    )
    if (file.exists(reference) && file.exists(result)) {
      run_check(
        paste(corpus, variant, "diff", sep = "-"),
        rscript,
        c(
          "tools/compare-rasterizer-corpus.R",
          reference,
          result,
          file.path(out, paste0(corpus, "-", variant, "-diff.csv"))
        )
      )
    }
  }
}
stopifnot(all(vapply(statuses, function(x) x$exit_code == 0, TRUE)))
