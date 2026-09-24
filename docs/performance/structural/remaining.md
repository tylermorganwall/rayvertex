# Remaining implementation and validation boundaries

The structural phases and the [subsequent continuation](../continuation/README.md) are implemented and measured. The brief also proposes experiments and deeper reorganizations that are not represented as complete here.

| Area | Still unimplemented |
|---|---|
| Scheduling/layout | Universal native sample layout and hardware-counter/false-sharing measurements. Optional spatial macrotiles now group unchanged coverage blocks; R matrices, outline arrays and tile fragments retain explicit separate layouts. |
| Geometry/bins | Unique-normal transform caching, retained submesh bounds, and a separate large-triangle list. Deterministic parallel count/prefix/fill now exists as an opt-in bounded-memory experiment; serial bins remain the default after mixed public-call results. |
| Optional interpolants | Per-material packed attribute indexing and individual normal specialization. Output matrices, tangent/intensity arrays, and common UV/view-position/shadow-clip arrays are demand-driven; common varying requirements use a conservative frame-wide union for mixed materials. |
| Visibility/coverage | Depth-prepass comparison, near-first traversal, hierarchical depth, and a new fixed-point/top-left edge rule. Conservative whole-block edge extrema are implemented as an opt-in experiment. Legacy inclusive edges and original ordering remain. General textured/unknown materials use the forward fallback; opacity metadata is not used to expand eligibility. |
| Shading | Algebraic replacement of the two tangent inversions, general virtual-call elimination and all material-specific shader kernels. The immutable input adapter retains legacy fragment bodies. A new coincident-point-light convention would be a separate correction; the current exceptional normalization is retained. |
| R output | Further fusion/parallelization of background and SSAO composition, tone mapping, bloom, resize and encoding. Exact RGBA assembly, sRGB decoding, orientation and final RGB clamp are fused in native helpers; rayimage metadata and the existing filters/effect order are preserved. |
| Prepared representation | A persistent fully packed native geometry copy, partial updates, shadow-map caching, persistent worker pools, and cross-frame environment variants. The implemented handle snapshots geometry/material textures and rebuilds frame-dependent state explicitly. |
| SIMD/precision | ISA-specific kernels/runtime dispatch and mixed precision. `Float` stays double; no global fast-math, native ISA flags or reduced sample quality is introduced. |

These omissions have no invented performance results. The new native profile justified geometry views; the measured SSAO regression justified explicit projection composition. Neither profile establishes that a tangent/SIMD/hierarchical-depth experiment wins. Such changes still need their own numerical/coverage contracts, portable fallback validation and before/after measurements.

Validation is extensive but not exhaustive. Windows, Linux and actual Emscripten builds, hardware counters, a supported leak detector, forced allocation failure/interrupt injection, every degenerate UV/extreme camera combination, OS-disk-cache cold, and plotting/file-encoding timings are not supplied. Native no-thread fallback, ASan/UBSan/TSan, exact release corpora, ownership counters, worker sweeps and representative quality sweeps are reported only where actually run. The upstream resize alignment UBSan diagnostic is retained as a failure, not counted as a clean sanitizer pass.
