# Validation commands

These commands belong to the initial implementation before homogeneous clipping.
For the current renderer and its corrected reference, use the
[structural validation report](structural/validation.md).

Run from the repository root, using separate installed libraries. The logs in
`validation/` record actual results; see README for the upstream UBSan failure
and platform limitations. The release package is 0.16.1; the system library also
contains an older 0.16.0, so every driver asserts the requested library path.

```sh
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars \
  R CMD INSTALL --preclean --library=/tmp/rayvertex-optimization/lib-delivery .
Rscript -e '.libPaths(c("/tmp/rayvertex-optimization/lib-delivery",.libPaths())); testthat::test_dir("tests/testthat",package="rayvertex",load_package="installed",reporter="summary",stop_on_failure=TRUE)'
Rscript tools/rasterizer-parity.R /tmp/rayvertex-optimization/lib-delivery /tmp/final.rds docs/performance/reference/parity-reference.rds
RAYVERTEX_INDEXED_TRANSFORMS=1 Rscript tools/rasterizer-parity.R /tmp/rayvertex-optimization/lib-delivery /tmp/indexed.rds docs/performance/reference/parity-reference.rds
```

Build a source archive and check it in a temporary directory:

```sh
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars \
  R CMD build --no-build-vignettes --no-manual /Users/tyler/Desktop/R/rayvertex
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars _R_CHECK_FORCE_SUGGESTS_=false \
  R CMD check --no-manual --no-build-vignettes rayvertex_0.16.1.tar.gz
```

Sanitizer builds use the recorded `Makevars-asan` or `Makevars-tsan` and
`R CMD INSTALL --preclean --no-test-load` into independent libraries. On this
macOS installation launch the actual R executable so its wrapper does not
remove the dynamic sanitizer runtime variable:

```sh
R_HOME=/Library/Frameworks/R.framework/Resources \
DYLD_INSERT_LIBRARIES=/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.asan_osx_dynamic.dylib \
ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
/Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave \
  -e '.libPaths(c("/tmp/rayvertex-optimization/lib-asan",.libPaths())); library(rayvertex); testthat::test_file("tests/testthat/test-rasterizer-correctness.R",stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-rasterizer-optimization.R",stop_on_failure=TRUE)'
```

The broad sanitizer parity diagnostic uses `UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=0`,
`RAYVERTEX_PARITY_TOLERANCE=1e-9`, and `source("tools/rasterizer-parity.R")`
with library/output/reference arguments after `--args`. This recover-mode run
**reports UBSan errors**; a successful R exit does not mean it passed UBSan.
TSan uses `libclang_rt.tsan_osx_dynamic.dylib`, `TSAN_OPTIONS=halt_on_error=1`
and `lib-tsan`, with the same two test files. Native no-thread builds use the
recorded `Makevars-serial` and run the parity driver without tolerance.

The standalone `tools/test-raster-utils.cpp` uses the system compiler with
`-std=c++17 -fsanitize=address,undefined` (the standalone Apple compiler otherwise
defaults to an older standard; the R package build supplies its standard). `tools/test-rayimage.cpp` additionally links
`src/rayimage.cpp`, includes R, Rcpp and the package's `src`/GLM headers, and
links libR. The standalone tests exercise the shared indexing/blur helpers and
channel-safe sampling without relying on an R image decoder's channel choices.

For native sampling, start `Rscript tools/profile-rasterizer.R LIB PID_FILE grid1m`,
wait for PID_FILE, then use `/usr/bin/sample PID 5 1 -file native-profile.txt`.
Do not run sampling, builds, tests or multiple renderer processes during timing
measurements. Hardware counters and a supported leak detector remain unmeasured.
