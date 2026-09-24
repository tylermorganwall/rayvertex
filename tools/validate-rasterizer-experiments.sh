#!/bin/bash
# Run sequentially after all timing processes have completed. Local macOS paths.
set -eu
run=/tmp/rayvertex-optimization
out="$run/experiments-accepted-validation"
mkdir -p "$out" "$run/lib-experiments-accepted-asan" "$run/lib-experiments-accepted-tsan" "$run/lib-experiments-accepted-serial" "$run/lib-experiments-final"
echo "build,test,exit_code" > "$out/sanitizer-status.csv"
Rscript tools/test-rasterizer-package.R "$run/lib-composition" "$out/release-tests.csv" > "$out/release-tests.log" 2>&1
for kind in asan tsan; do
  R_MAKEVARS_USER="$run/Makevars-$kind" R CMD INSTALL --preclean --no-test-load --library="$run/lib-experiments-accepted-$kind" . > "$out/build-$kind.log" 2>&1
  for name in correctness optimization parallel-bins macrotiles coverage-bounds varyings output normal-cache composition prepared; do
    testfile="tests/testthat/test-rasterizer-$name.R"
    if [ "$name" = prepared ]; then testfile=tests/testthat/test-prepared-scene.R; fi
    set +e
    R_HOME=/Library/Frameworks/R.framework/Resources DYLD_INSERT_LIBRARIES="/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.${kind}_osx_dynamic.dylib" ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 TSAN_OPTIONS=halt_on_error=1 /Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave -e '.libPaths(c(commandArgs(TRUE)[1],.libPaths())); library(rayvertex); testthat::test_file(commandArgs(TRUE)[2],stop_on_failure=TRUE)' --args "$run/lib-experiments-accepted-$kind" "$testfile" > "$out/$kind-$name.log" 2>&1
    status=$?
    set -e
    echo "$kind,$name,$status" >> "$out/sanitizer-status.csv"
  done
  set +e
  R_HOME=/Library/Frameworks/R.framework/Resources DYLD_INSERT_LIBRARIES="/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.${kind}_osx_dynamic.dylib" ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=0 TSAN_OPTIONS=halt_on_error=1 RAYVERTEX_PARITY_TOLERANCE=1e-9 RAYVERTEX_NORMAL_CACHE=1 RAYVERTEX_BLOCK_COVERAGE=1 RAYVERTEX_PARALLEL_BINS=1 RAYVERTEX_MACROTILE_EDGE=32 RAYVERTEX_VISIBILITY=1 RAYVERTEX_PARITY_PREPARED=1 /Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave -e 'source("tools/rasterizer-parity.R")' --args "$run/lib-experiments-accepted-$kind" "$out/$kind-parity.rds" docs/performance/structural/final-reference/reference.rds > "$out/$kind-parity.log" 2>&1
  status=$?
  set -e
  echo "$kind,combined-parity,$status" >> "$out/sanitizer-status.csv"
done
R_MAKEVARS_USER="$run/Makevars-serial" R CMD INSTALL --preclean --library="$run/lib-experiments-accepted-serial" . > "$out/build-serial.log" 2>&1
RAYVERTEX_TEST_NO_THREADS=1 Rscript tools/test-rasterizer-package.R "$run/lib-experiments-accepted-serial" "$out/serial-tests.csv" > "$out/serial-tests.log" 2>&1
RAYVERTEX_NORMAL_CACHE=1 RAYVERTEX_BLOCK_COVERAGE=1 RAYVERTEX_PARALLEL_BINS=1 RAYVERTEX_MACROTILE_EDGE=32 RAYVERTEX_VISIBILITY=1 RAYVERTEX_PARITY_PREPARED=1 Rscript tools/rasterizer-parity.R "$run/lib-experiments-accepted-serial" "$out/serial-parity.rds" docs/performance/structural/final-reference/reference.rds > "$out/serial-parity.log" 2>&1
R_MAKEVARS_USER="$run/Makevars" R CMD INSTALL --preclean --library="$run/lib-experiments-final" . > "$out/build-final-release.log" 2>&1
RAYVERTEX_NORMAL_CACHE=1 RAYVERTEX_BLOCK_COVERAGE=1 RAYVERTEX_PARALLEL_BINS=1 RAYVERTEX_MACROTILE_EDGE=32 RAYVERTEX_VISIBILITY=1 RAYVERTEX_PARITY_PREPARED=1 Rscript tools/rasterizer-parity.R "$run/lib-experiments-final" "$out/release-parity.rds" docs/performance/structural/final-reference/reference.rds > "$out/release-parity.log" 2>&1
RAYVERTEX_NORMAL_CACHE=1 RAYVERTEX_PARITY_PREPARED=1 Rscript tools/rasterizer-shader-parity.R "$run/lib-experiments-final" "$out/release-shader-parity.rds" "$run/continuation-varying-validation/shader-reference.rds" > "$out/release-shader-parity.log" 2>&1
