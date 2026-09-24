# Final structural validation

Renderer `d48a259`, Apple M1 Max / macOS 15.7.3 / R 4.6.0 / Apple clang 16. Release, ASan/UBSan, TSan and native no-thread builds use separate installed libraries and the recorded [Makevars files](validation/). The final release rebuild restores ordinary threaded object files after the sanitizer/no-thread builds. Tests and benchmarks run sequentially. No global fast-math or native ISA flags were introduced.

## Outcomes

| Check | Observed result |
|---|---|
| Full installed release source suite | 1,102 assertions passed; zero failures, warnings or skips. [Per-test timings](validation/release-tests.csv), [log](validation/release-tests.log). |
| Source archive `R CMD check --no-manual --no-build-vignettes` | Zero errors/warnings; one existing NOTE for unqualified `tail()` in `sweep_mesh_data`. Examples passed. Packaged tests: 1,090 assertions passed, seven CRAN skips. [Check log](validation/00check.log), [test log](validation/package-tests.Rout). |
| Final scalar corpus | 245 render/debug results exact against the clipped scalar corpus. Default final renderer and combined prepared/visibility/indexed rendering match it; combined experimental run uses ten workers. |
| Ordinary delivery table | 238/238 image/buffer comparisons exact, including attributes and nonfinite classifications. |
| Worker / quality / visibility sweeps | 272/272, 187/187 and 102/102 image/buffer comparisons exact, respectively. |
| Extended shader corpus | 44 debug-all results, containing 704 buffers, exact against `e512c02` for ordinary final and combined prepared/visibility/indexed final rendering. [Ordinary differences](validation/shader-differences-final.csv), [combined differences](validation/shader-differences-prepared.csv). |
| Standalone ASan/UBSan | Index/blur helpers, texture sampling, fragment arena, homogeneous clipping, triangle setup/bins, shadow PCF and point-light samples passed. Exact randomized checks include 750,000 tree/arena inserts, 523,260 PCF comparisons and 100,000 light samples. |
| Focused package ASan/UBSan | Correctness, optimization and prepared-scene tests passed, with the prepared fork test skipped by its CRAN guard. The added 18 Oren-Nayar/shader assertions also passed. |
| Broad package ASan/UBSan | **Not clean.** The 245-result corpus passes an absolute `1e-9` cross-build tolerance (sanitizer O1 versus release O2), but recover-mode UBSan reports a misaligned 64-bit store in installed upstream `stb_image_resize2.h:3735`. [Full diagnostic](validation/asan-parity-recover.log). A successful R exit does not count as a sanitizer pass. |
| Focused ThreadSanitizer | Correctness, optimization, environment-asset and prepared-scene tests passed; prepared fork test skipped by its CRAN guard. The added 18 shader assertions passed separately. |
| Native no-thread fallback | All 245 results exact, both ordinary and prepared+visibility modes. This is a native no-thread build, not execution in Emscripten. |

The extended corpus exercises all six public shader names, alpha-textured color/diffuse/Phong variants, Oren-Nayar sigma 1/45/90 and both sidedness settings, object/tangent normal mapping, two directional lights plus a point light, shadows on/off, and 7×5 shadow-map borders. Output is 59×37 with FSAA 2. Its generator and per-buffer differences are retained; SHA-256 hashes identify the larger temporary reference/final/prepared RDS files in `/tmp/rayvertex-optimization`. The principal 245-result golden corpora remain committed, with the final scalar source branch at `811143b`.

Prepared-scene tests exercise source-list and texture-file mutation/deletion, explicit rebuilding, callbacks/camera/lights/effects, invalid serialized/forked handles, partial decode failure and ten create/render/release cycles. Native live-handle/texture-byte counters return to baseline. These ownership checks and RSS measurements are not a supported leak-detector result; LeakSanitizer is disabled on this macOS setup.

Windows/Linux, actual Emscripten, hardware counters, forced allocation failures/interrupt injection, every extreme camera or degenerate UV combination, OS-disk-cache-cold timing, plotting and file encoding remain untested or unmeasured. The finite corpus is not proof for all inputs. See [remaining scope](remaining.md).

## Reproduction

Run from the repository root. Create isolated library directories before installation. The archived [sanitizer/no-thread script](validation/validate-structural.sh) and [Makevars files](validation/) record the actual machine-specific compiler and runtime commands. `--preclean` is required after header or compiler-flag changes. Replace machine paths when reproducing elsewhere; do not add these temporary toolchain overrides to package requirements.

```sh
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars \
  R CMD INSTALL --preclean --library=/tmp/rayvertex-optimization/lib-screen-final .
Rscript -e '.libPaths(c("/tmp/rayvertex-optimization/lib-screen-final",.libPaths())); testthat::test_dir("tests/testthat",package="rayvertex",load_package="installed",reporter="summary",stop_on_failure=TRUE)'
Rscript tools/rasterizer-parity.R \
  /tmp/rayvertex-optimization/lib-screen-final /tmp/final.rds \
  docs/performance/structural/final-reference/reference.rds
RAYVERTEX_PARITY_PREPARED=1 RAYVERTEX_VISIBILITY=1 RAYVERTEX_INDEXED_TRANSFORMS=1 \
  RAYVERTEX_PARITY_CORES=10 Rscript tools/rasterizer-parity.R \
  /tmp/rayvertex-optimization/lib-screen-final /tmp/experimental.rds \
  docs/performance/structural/final-reference/reference.rds
Rscript tools/rasterizer-shader-parity.R \
  /tmp/rayvertex-optimization/lib-clipped /tmp/shaders-reference.rds
Rscript tools/rasterizer-shader-parity.R \
  /tmp/rayvertex-optimization/lib-screen-final /tmp/shaders-final.rds /tmp/shaders-reference.rds
RAYVERTEX_PARITY_PREPARED=1 RAYVERTEX_VISIBILITY=1 RAYVERTEX_INDEXED_TRANSFORMS=1 \
  Rscript tools/rasterizer-shader-parity.R \
  /tmp/rayvertex-optimization/lib-screen-final /tmp/shaders-prepared.rds /tmp/shaders-reference.rds
```

Build the archive and run the check from a temporary directory, so the source tree stays free of generated archives/check directories:

```sh
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars \
  R CMD build --no-build-vignettes --no-manual /Users/tyler/Desktop/R/rayvertex
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars _R_CHECK_FORCE_SUGGESTS_=false \
  R CMD check --no-manual --no-build-vignettes rayvertex_0.16.1.tar.gz
```

The additional shader test is `testthat::test_file("tests/testthat/test-rasterizer-shader-variants.R", stop_on_failure=TRUE)` under each sanitizer's recorded launcher. The standalone texture check uses the following command. Include `src/glm`, not `src` in the system header search path: the repository's unrelated `src/assert.h` otherwise shadows the standard assertion header (the first standalone invocation hit this compile error; correcting the invocation passed).

```sh
clang++ -std=c++17 -fsanitize=address,undefined -fno-omit-frame-pointer -g \
  -Isrc/glm -I/Library/Frameworks/R.framework/Resources/include \
  -I/Library/Frameworks/R.framework/Versions/4.6/Resources/library/Rcpp/include \
  tools/test-rayimage.cpp src/rayimage.cpp \
  -L/Library/Frameworks/R.framework/Resources/lib -lR \
  -Wl,-rpath,/Library/Frameworks/R.framework/Resources/lib \
  -o /tmp/rayvertex-optimization/test-rayimage-final
ASAN_OPTIONS=detect_leaks=0 /tmp/rayvertex-optimization/test-rayimage-final
```

Benchmark drivers assert the loaded library and retain their actual settings, compiler/session information, raw timings, phase diagnostics and differences. `tools/bench-rasterizer-structural.py` runs the phase/public-memory comparison; `tools/bench-rasterizer-structural-matrix.py` runs worker/quality/visibility sweeps; `tools/bench-rasterizer-regression-loops.py` repeats the public regressions. Do not run benchmarks alongside tests, compilation, profiling or another renderer. P95 from three samples is descriptive, not a stable tail estimate. The stopped pre-SSAO-fix sweep is diagnosis only; final sweeps were rerun on `d48a259`.

The full final measurements were generated with these drivers. Start with new output directories and clear developer feature switches, as listed in the parent README. The `lib-clipped` library is built from `e512c02`; `lib-screen-final` is built from `d48a259`. Sweep drivers alternate variant order and select visibility explicitly for the visibility suite.

```sh
RAYVERTEX_STRUCTURAL_CASES=small,grid100k,grid500k,grid1m,alpha4,alpha16,alpha64,shadow,ssao,toon,shared_textures,environment,environment_shared,point_lights \
  python3 tools/bench-rasterizer-structural.py \
  /tmp/rayvertex-optimization/lib-clipped /tmp/rayvertex-optimization/lib-screen-final \
  /tmp/rayvertex-optimization/structural-delivery
python3 tools/bench-rasterizer-structural-matrix.py \
  /tmp/rayvertex-optimization/lib-clipped /tmp/rayvertex-optimization/lib-screen-final \
  /tmp/rayvertex-optimization/structural-workers-final workers
python3 tools/bench-rasterizer-structural-matrix.py \
  /tmp/rayvertex-optimization/lib-clipped /tmp/rayvertex-optimization/lib-screen-final \
  /tmp/rayvertex-optimization/structural-quality-final quality
python3 tools/bench-rasterizer-structural-matrix.py \
  /tmp/rayvertex-optimization/lib-screen-final /tmp/rayvertex-optimization/lib-screen-final \
  /tmp/rayvertex-optimization/structural-visibility-expanded visibility
python3 tools/bench-rasterizer-regression-loops.py \
  /tmp/rayvertex-optimization/lib-clipped /tmp/rayvertex-optimization/lib-screen-final \
  /tmp/rayvertex-optimization/structural-regression-loops
```
