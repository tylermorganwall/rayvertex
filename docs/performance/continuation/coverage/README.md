# Conservative block coverage experiment

Source `dbaf8b1`, same release build with `RAYVERTEX_BLOCK_COVERAGE` unset versus `1`. Default remains off. [Measurements](results.md) retain every case, including regressions.

For bounding-box intersections with at least 16 samples, the kernel evaluates the existing floating-point edge expression at four corners. Finite monotonic extrema conservatively identify wholly rejected or accepted blocks for the existing inclusive winding/culling rule. Partial and exceptional blocks use the scalar path. Full blocks retain original per-sample weights, depth and shading. The disabled template has no added inner-loop branch. Main and shadow passes share the implementation; visibility replay retains the same setting.

Thin triangles benefit: at one worker native/public medians were 51/66 → 33/48 ms. Their diagnostic reduced explicit edge comparisons from 20,230,943 candidates to 532,495 samples; 1,231,153 of 1,246,181 classified blocks were rejected. Logical candidate, covered, shader and transparency counts match for all 15 settings (60 counter comparisons). These counts describe avoided work, not a hardware instruction measurement.

Costs are mixed. The HD grid native median increased 95 → 101 ms while its public median was 1200 → 1196 ms. At that setting only 8,499 of 139,231 tested blocks were rejected and 1,098 fully accepted. Fresh RSS was 1733.7 → 1772.8 MiB; this phase adds counters and stack-local extrema, not a persistent frame buffer. The RSS increase is reported, not assigned a proven cause. No universal default or adaptive selector is justified by this sweep.

Validation: 27,018,750 scalar sample classifications under ASan/UBSan, 30 focused assertions, and 245 exact corrected-reference render/debug results with prepared assets, visibility, parallel bins and macrotiles enabled. The matrix has 255/255 exact image/buffer comparisons, maximum absolute difference zero. [Validation logs](../coverage-validation/) include the standalone random/extreme finite and nonfinite fallback cases. One diagnostic sample supplies per-phase counters/timings; three warm/native calls do not establish a reliable p95 tail.
