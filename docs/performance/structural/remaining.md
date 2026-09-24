# Remaining implementation and validation boundaries

The structural phases above are implemented and measured. The brief also proposes experiments and deeper reorganizations that are not represented as complete here.

| Area | Still unimplemented |
|---|---|
| Scheduling/layout | A separate spatial macrotile/microblock hierarchy, universal native sample layout, and hardware-counter/false-sharing measurements. Existing coverage blocks are batched; R matrices, outline arrays and tile fragments retain explicit separate layouts. |
| Geometry/bins | Unique-normal transform caching, retained submesh bounds, deterministic parallel bin construction, and a separate large-triangle list. Bins use serial count/prefix/fill; the profile showed substantially more vertex-access work, which was addressed first. |
| Optional interpolants | Unused output matrices and tangent/intensity arrays are removed, but common per-face UV/position/normal arrays are not individually specialized for every material/consumer combination. |
| Visibility/coverage | Depth-prepass comparison, near-first traversal, whole-block edge extrema, hierarchical depth, and a new fixed-point/top-left edge rule. Legacy inclusive edges and original ordering remain. General textured/unknown materials use the forward fallback; opacity metadata is not used to expand eligibility. |
| Shading | Algebraic replacement of the two tangent inversions, general virtual-call elimination and all material-specific shader kernels. The immutable input adapter retains legacy fragment bodies. A new coincident-point-light convention would be a separate correction; the current exceptional normalization is retained. |
| R output | Fusion or parallelization of R-side color conversion, reorientation, tone mapping, bloom, resize and encoding. At 1920×1080/FSAA 2 these costs dominate several public-call measurements. No filtering or output metadata was bypassed. |
| Prepared representation | A persistent fully packed native geometry copy, partial updates, shadow-map caching, persistent worker pools, and cross-frame environment variants. The implemented handle snapshots geometry/material textures and rebuilds frame-dependent state explicitly. |
| SIMD/precision | ISA-specific kernels/runtime dispatch and mixed precision. `Float` stays double; no global fast-math, native ISA flags or reduced sample quality is introduced. |

These omissions have no invented performance results. The new native profile justified geometry views; the measured SSAO regression justified explicit projection composition. Neither profile establishes that a tangent/SIMD/hierarchical-depth experiment wins. Such changes still need their own numerical/coverage contracts, portable fallback validation and before/after measurements.

Validation is extensive but not exhaustive. Windows, Linux and actual Emscripten builds, hardware counters, a supported leak detector, forced allocation failure/interrupt injection, every degenerate UV/extreme camera combination, OS-disk-cache cold, and plotting/file-encoding timings are not supplied. Native no-thread fallback, ASan/UBSan/TSan, exact release corpora, ownership counters, worker sweeps and representative quality sweeps are reported only where actually run. The upstream resize alignment UBSan diagnostic is retained as a failure, not counted as a clean sanitizer pass.
