> Historical report for the first implementation through `18751b4`. See [README.md](README.md) for the structural continuation and current limits.

# CPU rasterizer implementation and measurement report

This is a **partial implementation** of the optimization brief. Default rendering now avoids disabled-pass storage, batches active coverage blocks, shares texture decodes within a frame, rejects hidden samples before perspective normalization, and parallelizes SSAO and outline propagation. The indexed-transform experiment is **off by default** because its end-to-end results are mixed. Public R signatures, default FSAA, shadow resolution, filtering, transparency layers, and PCF sampling have not been reduced.

The measured workload tables are in [results.md](results.md), with before/after per-phase tables in [phase-results.md](phase-results.md), [phase decisions](phase-interpretation.md), and [remaining workload costs](workload-limits.md). Raw samples, phase timings, counters, R allocations and process RSS are retained under `raw/`. These are measurements on one machine, not portable performance guarantees. Changed-file lists for each implementation commit are in [changes-by-commit.txt](changes-by-commit.txt).

## Source and environment

The starting checkout was clean at `b09385978f40f976e7e97286191d64d16184b7de`. Instrumentation-only commit `57568ca` supplies the measured phase-0 baseline. Correctness-only branch `codex/raster-scalar-reference`, commit `7f72368`, retains the corrected scalar implementation: ownership/indexing fixes plus subsequent shader initialization corrections, with none of the performance phases. Its 245 small render/debug outputs are retained in `reference/parity-reference.rds`. The expanded measurements use renderer checkpoint `3bb9586`. Delivery commit `18751b4` subsequently corrects a profiler-only tangent clip count and adds its regression test; rendering algorithms are the same. Its separate timing/overhead check is in [delivery-diagnostic.md](delivery-diagnostic.md).

Apple M1 Max, 10 physical/logical cores (8 performance and 2 efficiency cores), 64 GiB RAM, macOS 15.7.3, native arm64 R 4.6.0, Apple clang 16.0.0. Effective release flags: `-falign-functions=64 -Wall -g -O2`; no fast-math or native-ISA flags. Dependency versions and session information are retained in `validation/`. The existing compiler cache could not start in the sandbox; a temporary Makevars bypassed it without changing optimization flags:

```make
CXX = $(CXX17) $(CXX17STD)
SHLIB_CXXLD = $(CXX)
CCACHE =
MAKEFLAGS = -j4
```

This is a build-local override, not a package C++ standard requirement.

## Measurement contract

Fixtures are deterministic and procedural; no downloads or random assets are needed. `tools/rasterizer-fixtures.R` is their specification. Grids have two triangles per cell, a small sinusoidal displacement, indexed positions, and approximately 100k, 500k or 1M triangles. The occlusion case stacks 16 Phong surfaces front to back. Transparency cases have 4, 16 or 64 surfaces. These layered surfaces are not a complete intersecting-geometry performance suite. `shared_textures` has 16 differently shaded cubes referencing one generated 512×512 RGB image, including one shadow pass.

Camera: `(0,0,4)` looking at `(0,0,0)`, 40-degree field of view. Ordinary `rasterize_scene()` calls use package-default lights, no plotting or encoding, and 256×256 shadow maps where enabled. Settings are identical within each comparison. Resolution, FSAA and workers are separate columns; FSAA 2 is never compared to FSAA 1 as an optimization.

`process_cold` means the first public render in a new R process, after package loading and fixture creation. It includes validation, merging, material preparation, decode, rasterization and R postprocessing. It excludes interpreter startup and fixture generation. Generated texture files are already in the OS cache. Neither this sample nor restarting R means disk-cache cold. Warm samples are subsequent complete public calls. Isolated native samples reuse captured arguments to the existing internal wrapper; they still allocate/decode/render/tear down each frame. They are **not a prepared-scene API or benchmark**.

An explicit GC runs before each timed sample, outside the interval. Native diagnostics and Rprofmem run separately from ordinary timings. These per-call FPS values exclude the explicit preceding GC and should not be treated as sustained application throughput. [Sustained API loops](sustained.md) separately measure automatic GC and include a final cleanup inside the total-loop timer. Final instrumentation-overhead samples also use GC before each sample. Earlier phase profiles did not include all native teardown in `native_total`; use the uninstrumented `*-native.csv` wall times for cross-phase native comparisons. Those include teardown throughout. Final instrumentation adds explicit `native_teardown` and `native_output_assembly` phases.

Median, interpolated sample p95, cold time, FPS and repetition counts are provided. Most matrix cases have five warm samples, alpha64 has three, focused early-Z runs have fifteen, and batch sweeps have ten. A p95 from three or five samples is descriptive, not a stable tail-latency estimate. No confidence interval or universal multiplier is claimed. Runs were sequential, without competing compilation, tests or renderer benchmarks. Variant order was not randomized; thermal/allocator drift can affect small differences. Additional alternating-order repeats are retained in [regression-repeats.md](regression-repeats.md); they show substantial variation, include every outlier, and do not establish a stable win for every workload.

`peak_rss_bytes` is whole benchmark-process high-water RSS: startup, fixtures, retained image/debug outputs, diagnostics and Rprofmem. It is not per-frame memory. `single_render_peak_rss_bytes` comes from a fresh process with one render and no debug corpus or diagnostics. Neither subtracts the R runtime. `R_allocation_bytes` counts allocated R heap bytes in one public call, not peak live memory or all C++ allocations. Native payload counters cover named buffers only. Repeated-process RSS can rise despite reduced native payload because of allocation order, GC and retained copies; regressions are retained in the tables.

## Reviewable implementation ledger

| Phase | Commits | Classification and implementation | Limits |
|---|---|---|---|
| 0 | `57568ca`, `3bb9586` | Pinned baseline, fixtures, optional native instrumentation/counters, timing/allocation/RSS drivers and parity tools | Partial fine-grained instrumentation and matrix; see gaps |
| 1 | `ac27b76`, `08eecd2`, `908b46a`, `2a90464` | Correctness: SSAO stride 4; stable blur source; independent presentation/linear depth; centralized transparency indexing; safe gray/gray-alpha sampling; RAII images/shaders; initialized shader transforms and fragment inputs | Clipping, edge ownership and orthographic depth formula retain legacy behavior |
| 2 | `49df13e`, compatibility follow-up `08cedf1` | Equivalent behavior: const-reference shader list, fixed three-corner attribute arrays, disabled shadow/outline allocation gates, material deduplication without vector growth | Not all auxiliary outputs are lazy; unnamed lists and -1 IDs preserved |
| 3 | `570ee6f` | Equivalent behavior: active block list, batches of 64 existing blocks, reused worker pool, barriers, direct serial dispatch | Blocks remain 4×4; no macro/microtile redesign or unified layout |
| 4.1 | `36e5df0` | Experimental equivalent behavior: indexed clip/view-position transforms preserving multiplication orders and independent UV/normal indices | Opt-in only; no homogeneous clipping, once-per-primitive setup, packed bins or normal-transform cache |
| 5.1 | `bddab0f` | Equivalent behavior: affine depth rejection before perspective-weight normalization | No opacity classes, visibility buffer, prepass, coverage extrema or hierarchical depth |
| 6.4 | `56c4b76` | Equivalent behavior: frame-local canonical-path cache; immutable decodes shared across materials and shadow shaders | Fixed existing float/original-channel decode options; no cross-frame cache, opacity metadata, tangent/light/PCF specialization or shared environment variants |
| 8 | `5c7bb8b` | Equivalent behavior: parallel SSAO regions/blur and JFA initialization/ping-pong iterations with barriers | Final outline composition, environment sampling, transparency resolve and R output conversions remain serial |
| 7, 9, 10 | — | Unimplemented | Sparse exact transparency; prepared scenes/invalidation; SIMD, hierarchical depth and mixed precision |

Correctness fixes discovered after initial phase-2/3 work were committed separately and replayed on the scalar branch. `0005fb1` fixes test helper order. Early phase tables use initial corrected checkpoint `ac27b76`; those nine fixtures did not exercise the subsequently corrected shader inputs. Final comparisons use fully corrected `7f72368`. Focused early-Z A/B measures the early-Z kernel patch on the phase-3 workload; corrected-reference parity was checked independently afterward.

The transform cache applies only to eligible meshes (at least 1,000 faces, fewer than twice as many positions as faces) when `RAYVERTEX_INDEXED_TRANSFORMS` is set. It preserves shader multiplication orders instead of factoring different floating-point expressions together. In the focused 500k case native median fell 218→208 ms while public-call median rose 405→435 ms. The 1M case improved 549→530 ms end to end but added 28.15 MB of transform payload. This does not support enabling it by default.

## Correctness and image differences

`phase1-differences.csv` records intentional baseline-to-corrected differences. Among nine timed fixtures, only SSAO changed: maximum final-image absolute difference 0.02433481; RGB debug 0.02113295; returned ambient 0.05209959. The original rectangular non-AA line regression segfaulted, and the original one-channel sampler had an ASan stack overread. Undefined outputs are not used as a numerical reference. Later shader fixes initialize undefined Model transforms, position/color/normal inputs and missing face-normal fallbacks; their prior undefined values also cannot define meaningful error bounds.

The corrected reference retains affine post-divide depth interpolation, inclusive shared edges, later-wins opaque equality, transparent equal-depth overwrite, triangle-before-line ordering, and unlimited transparent layer growth. Internal depth is separate from presentation depth. Returned depth/background and the existing linear-depth formula, including orthographic behavior, are preserved. This work does not claim to fix legacy near-plane clipping, far-plane occupancy or edge ownership.

Every measured equivalent phase has a `phaseN-differences.csv`: image and all debug buffers, exact metadata/attributes, maximum/mean absolute error, changed samples, nonfinite mismatches and worst linear index. The intentional SSAO correction is illustrated in [correction-ssao.png](correction-ssao.png). All equivalent-phase comparisons are exact; the expanded matrix has 663 exact image/buffer comparisons across 39 settings, with zero attribute mismatches. The 245-result corpus is also exact for final release, opt-in transforms and the compiled no-thread fallback. It covers shader families, all debug modes, transparent backgrounds, odd/portrait dimensions, FSAA 2, near-plane/offscreen/sliver geometry, independent UV/normal indices, normal maps, multiple lights, refraction-only environment use, both line algorithms, transparency and 129-layer stress. This asserts compatibility, not mathematical correctness of every legacy algorithm.

Focused tests check worker determinism/equality ordering, SSAO traversal independence, disabled allocations, a 72-covered-sample quad (64 pixels plus the legacy shared diagonal), shared decoding, material-list attributes, texture-load exception cleanup and transform callback counts. Standalone sampler tests cover 1/2/3/4 channels, 1×1/narrow textures, negative/wrapped UVs, fractional filtered alpha and HDR values.

## Validation and remaining risks

Final `R CMD build` and `R CMD check --no-manual --no-build-vignettes` completed: 988 test passes, 0 failures, 0 test warnings, 6 skips; package check reports one NOTE for existing unqualified `tail()` calls in `R/sweep_mesh.R`. Examples passed. The installed delivery build also passes all 999 expectations with `NOT_CRAN=true` against the source test suite, without those skips. Repository metadata endpoints were unreachable in the sandbox; installed dependencies were used. Skips and complete output are in `validation/`. Windows, Linux and actual Emscripten builds were not run. The `RAYVERTEX_NO_THREADS` native fallback compiled and passed exact parity. No mandatory new thread runtime or ISA requirement was introduced.

ASan+UBSan focused regression tests and standalone utility/sampler tests pass. ThreadSanitizer passes correctness and optimization tests, including parallel SSAO/outlines. The broad ASan/UBSan corpus reports an alignment violation in installed dependency `stbimageheaders/include/stbimageheaders/stb_image_resize2.h:3735`: a misaligned `uint64` store during resize coefficient preparation. **The broad UBSan run is not clean.** Its recover-mode diagnostic is retained; the dependency issue was not silenced or patched as an unrelated optimization.

The sanitizer build uses `-O1`, release `-O2`. A tangent path differs by at most 2.328306e-10 between them; broad sanitizer parity uses explicit per-element absolute tolerance 1e-9, with exact attributes and nonfinite patterns. Every release comparison uses exact equality. LeakSanitizer was unavailable/disabled on this macOS setup. A 100-render refraction-only high-water experiment grew 96.1→299.5 MB on the original build and 93.7→150.1 MB after RAII fixes; growth after render 20 was much smaller after correction. This supports the ownership fix but is not proof of zero leaks.

A five-second native sample (`validation/native-profile.txt`) used repeated isolated 1M-triangle calls, four workers, final default code. Prominent non-wait top-of-stack samples: `ModelInfo::vertex` (1111), `fill_tri_blocks_impl` (1035), clearing (597), image writes (363), `DiffuseShader::vertex` (324). These are stacks sampled across threads, not elapsed phase percentages or hardware counters. They support further geometry/coverage/layout work, without establishing that an unimplemented SIMD or tangent design wins.

## Reproduction and developer controls

Use separate installed libraries for each revision. Create source/library directories, export the reference using `git archive 7f72368 | tar -x -C /tmp/rayvertex-reference-src`, then `R CMD INSTALL --preclean --library=/tmp/rayvertex-reference-lib /tmp/rayvertex-reference-src`. Install final source separately. Supply `R_MAKEVARS_USER` if the compiler-cache override above is needed. Do not mix variants in one library.

```sh
python3 tools/bench-rasterizer.py LIB OUTPUT_DIR grid100k 800 800 1 1 10
python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_DIR representative
python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_DIR quality
python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_DIR workers
python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_DIR memory
python3 tools/bench-rasterizer-matrix.py LIB OUTPUT_DIR batches
python3 tools/summarize-rasterizer.py OUTPUT_DIR SUMMARY_DIR
python3 tools/bench-rasterizer-repeats.py REFERENCE_LIB FINAL_LIB REPEAT_DIR
python3 tools/bench-rasterizer-sustained.py REFERENCE_LIB FINAL_LIB SUSTAINED_DIR
Rscript tools/compare-rasterizer.R BEFORE_DIR AFTER_DIR differences.csv
Rscript tools/rasterizer-parity.R LIB parity.rds reference.rds
```

The matrix driver skips completed cases; use a new directory to repeat. `RAYVERTEX_DIAGNOSTIC_REPS` controls separate instrumented repetitions. `RAYVERTEX_PROFILE=/path/to/file.csv` enables native diagnostics; unset it for timing. Developer controls `RAYVERTEX_REFERENCE_SCHEDULER`, `RAYVERTEX_REFERENCE_SCREEN`, `RAYVERTEX_REFERENCE_ASSETS`, `RAYVERTEX_REFERENCE_TRANSFORMS` force corresponding reference behavior when present. `RAYVERTEX_BATCH_BLOCKS` accepts 1–65536, default 64. Clear diagnostic controls before normal measurements. These are not public R API additions.

## Gaps relative to the full brief

Instrumentation separates allocation, combined shader setup/decode, model setup, shadow work, combined transform/setup/binning, combined coverage/depth/shading, SSAO, combined lines/resolve, environment, depth conversion, outlines and output. R callbacks provide coarse preparation/postprocessing intervals. Decode versus shader setup, clipping versus binning, coverage versus shading, individual shadow maps, lines versus resolve, and each R postprocessing step are **not fully separated**. Post-clip/cull, maximum-layer and shadow-tap counters are absent. Payload counts do not replace total native allocation tracking.

The timing matrix covers twelve representative 800×800 cases, selected 800×800/1920×1080 FSAA 1/2 cases, selected 1/2/4/10-worker cases and batches 16/64/256. It is not every shader/geometry/resolution combination. Tangent seams, 129 layers, lines, odd/portrait dimensions, multiple shadows and near-plane cases are primarily correctness tests. Coverage tile sizes were not swept because the block layout is unchanged. Plotting/encoding, OS-disk-cache cold, prepared throughput and prepared-scene break-even were not measured. No prepared API exists here. The corpus does not exhaust every winding/culling combination, every boundary-line endpoint ordering, degenerate UV configuration, extreme world/camera range, nonfinite input, or interrupt/allocation-failure injection. Prepared-scene and ISA-dispatch tests are unavailable because those features were not implemented.

Remaining structural work needs separate correctness contracts and measured review: homogeneous clipping/setup and packed bins; opaque visibility/opacity classification; tangent/light/shadow specialization; sparse exact transparency including line/shadow producers; remaining screen/output passes; prepared-scene lifetime/invalidation; and profile-guided SIMD/hierarchical depth. These phases are not represented as complete or as measured speedups.
