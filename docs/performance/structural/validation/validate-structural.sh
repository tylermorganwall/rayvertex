#!/bin/bash
set -eu
root=/Users/tyler/Desktop/R/rayvertex
run=/tmp/rayvertex-optimization
cd "$root"
mkdir -p "$run/lib-structural-asan" "$run/lib-structural-tsan" "$run/lib-structural-serial" "$run/structural-validation"
for test in raster-utils fragment-arena triangle-bins homogeneous-clip shadow-sampling; do
  clang++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc/glm "tools/test-$test.cpp" -o "$run/test-$test-final"
  "$run/test-$test-final" > "$run/structural-validation/$test.log" 2>&1
done
clang++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc/glm tools/test-light-sample.cpp src/light.cpp -o "$run/test-light-sample-final"
"$run/test-light-sample-final" > "$run/structural-validation/light-sample.log" 2>&1
R_MAKEVARS_USER="$run/Makevars-asan" R CMD INSTALL --preclean --no-test-load --library="$run/lib-structural-asan" . > "$run/structural-validation/build-asan.log" 2>&1
R_HOME=/Library/Frameworks/R.framework/Resources DYLD_INSERT_LIBRARIES=/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.asan_osx_dynamic.dylib ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 /Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave -e '.libPaths(c("/tmp/rayvertex-optimization/lib-structural-asan",.libPaths())); library(rayvertex); for(f in c("correctness","optimization")) testthat::test_file(paste0("tests/testthat/test-rasterizer-",f,".R"),stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-prepared-scene.R",stop_on_failure=TRUE)' > "$run/structural-validation/asan-focused.log" 2>&1
R_HOME=/Library/Frameworks/R.framework/Resources DYLD_INSERT_LIBRARIES=/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.asan_osx_dynamic.dylib ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=0 RAYVERTEX_PARITY_TOLERANCE=1e-9 /Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave -e 'source("tools/rasterizer-parity.R")' --args "$run/lib-structural-asan" "$run/parity-structural-asan.rds" "$run/parity-clipped-scalar.rds" > "$run/structural-validation/asan-parity-recover.log" 2>&1
R_MAKEVARS_USER="$run/Makevars-tsan" R CMD INSTALL --preclean --no-test-load --library="$run/lib-structural-tsan" . > "$run/structural-validation/build-tsan.log" 2>&1
R_HOME=/Library/Frameworks/R.framework/Resources DYLD_INSERT_LIBRARIES=/Library/Developer/CommandLineTools/usr/lib/clang/16/lib/darwin/libclang_rt.tsan_osx_dynamic.dylib TSAN_OPTIONS=halt_on_error=1 /Library/Frameworks/R.framework/Resources/bin/exec/R --vanilla --slave -e '.libPaths(c("/tmp/rayvertex-optimization/lib-structural-tsan",.libPaths())); library(rayvertex); for(f in c("correctness","optimization","environment-assets")) testthat::test_file(paste0("tests/testthat/test-rasterizer-",f,".R"),stop_on_failure=TRUE); testthat::test_file("tests/testthat/test-prepared-scene.R",stop_on_failure=TRUE)' > "$run/structural-validation/tsan-focused.log" 2>&1
R_MAKEVARS_USER="$run/Makevars-serial" R CMD INSTALL --preclean --library="$run/lib-structural-serial" . > "$run/structural-validation/build-serial.log" 2>&1
Rscript tools/rasterizer-parity.R "$run/lib-structural-serial" "$run/parity-structural-serial.rds" "$run/parity-clipped-scalar.rds" > "$run/structural-validation/serial-parity.log" 2>&1
RAYVERTEX_PARITY_PREPARED=1 RAYVERTEX_VISIBILITY=1 Rscript tools/rasterizer-parity.R "$run/lib-structural-serial" "$run/parity-structural-serial-prepared.rds" "$run/parity-clipped-scalar.rds" > "$run/structural-validation/serial-prepared-parity.log" 2>&1
