# Accepted renderer validation

Accepted renderer: `f1d7e1226821f80cbde1ad1c855911f05435f64b`, source-equivalent to `5eb61c63` ignoring whitespace after removal of the tangent candidate. The corrected scalar branches and committed golden corpus are unchanged. Builds, tests, profiles and measured renders ran sequentially. The final library and workspace objects were rebuilt with the normal O2 flags after sanitizer/no-thread runs.

| Check | Result |
|---|---|
| Complete installed-source release tests | 1,412 assertions pass; zero failures/warnings/skips |
| New normal reuse tests | 74 assertions pass |
| New composition tests | 28 assertions pass; existing 42 output assertions also pass |
| Final O2 reference corpus | 245 render/debug outputs exact, including prepared scenes with normal reuse and the earlier optional structural paths combined |
| Extended O2 shader corpus | 44 shader/debug outputs exact with prepared scenes and normal reuse |
| Accepted ordinary timing comparisons | 136/136 image/buffer comparisons exact |
| Accepted prepared timing comparisons | 51/51 exact |
| Composition HD/FSAA-2 comparisons | 51/51 exact |
| ASan/UBSan focused suite | 464 assertions pass, with `halt_on_error=1` |
| TSan focused suite | 464 assertions pass, with `halt_on_error=1` |
| TSan broad corpus | All 245 outputs meet the pre-existing cross-build absolute tolerance of 1e-9; no race diagnostic |
| ASan/UBSan broad corpus | Numerical comparison passes at 1e-9; **sanitizer failure remains** in upstream resize code, described below |
| `RAYVERTEX_NO_THREADS` build | 1,392 applicable assertions pass; all 245 combined-path outputs match exactly |
| `R CMD check --no-manual --no-build-vignettes` | Zero errors/warnings; one pre-existing NOTE about unqualified `tail()` in `R/sweep_mesh.R` |

The packaged check passes 1,411 assertions and skips the existing OBJ fixture test because the existing build exclusions omit that fixture. The installed-source test run includes it and passes all 1,412 assertions. The no-thread conditional omits 20 threaded assertions; it does not represent actual Emscripten execution. Repository-index access warnings appear during dependency checking under restricted network access; the dependency check completes OK.

## Known sanitizer failure and rejected candidate

The accepted ASan/UBSan broad corpus reproduces a misaligned `stbir_uint64` store in installed `stbimageheaders/stb_image_resize2.h:3735`, reached through reflection-map resizing. This is the same dependency failure recorded before these experiments. The broad run deliberately uses UBSan recovery to finish comparisons, so its zero process exit code is **not** a clean sanitizer pass. `asan-parity.log` retains the full diagnostic. LeakSanitizer is unavailable on this macOS configuration (`detect_leaks=0`); no successful leak check is claimed.

The tangent candidate was independently rejected: its integrated ASan O1 path differed from the same build's scalar tangent path in normal debug buffers, with maximum absolute difference 3.7252902984619141e-9. The scalar path matches the preceding ASan corpus exactly across all 770 leaf buffers. An explicit float-temporary trial did not resolve the mismatch. The candidate also lacked a public-call speed benefit. Its code, focused tests, timings and [failure evidence](../tangent/rejection/) are archived; the accepted renderer restores the original tangent bodies. Release images were exact for the candidate, but that did not override the failed sanitizer comparison. No tolerance was increased to accept it.

The archived standalone arithmetic test passes 1,000,012 cases at O1 and O2 under ASan/UBSan (NaNs compare by class, other components by bytes). The integrated failure demonstrates the limit of that test; it is not evidence that the rejected renderer was equivalent.

## Reproduction

`tools/validate-rasterizer-experiments.sh` contains the exact local sanitizer runtime launchers, options, test list, no-thread build and final O2 rebuild/reference commands. The Makevars files alongside this report record effective flags. Sanitizers use clang O1; release/no-thread use O2. Public quality settings remain fixed. No new package compiler-standard requirement, fast-math flag, native ISA flag or thread runtime is introduced.

The final release uses `/tmp/rayvertex-optimization/lib-experiments-final`. The full-source tests also ran against the source-equivalent immutable `lib-composition` install. To repeat the package check from a temporary directory:

```sh
R CMD build --no-build-vignettes --no-manual /Users/tyler/Desktop/R/rayvertex
R_MAKEVARS_USER=/tmp/rayvertex-optimization/Makevars \
R_LIBS_USER=/tmp/rayvertex-optimization/lib-experiments-final \
NOT_CRAN=true _R_CHECK_FORCE_SUGGESTS_=false \
R CMD check --no-manual --no-build-vignettes rayvertex_0.16.1.tar.gz
```

Compile the archived arithmetic experiment independently with `clang++ -std=c++17 -O2 -fsanitize=address,undefined -fno-omit-frame-pointer -g -Isrc/glm tools/test-tangent-basis.cpp`; run with `ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=halt_on_error=1`. Do not add `-Isrc`, which would shadow the system assert header. It is not linked into the package.

Windows, Linux, actual Emscripten, hardware counters, supported leak detection, forced allocation-failure/interrupt injection, OS-disk-cache-cold and plotting/encoding performance remain untested. Logs report only the checks actually performed.
