# Workload limits observed in this implementation

Interpretation uses combined native phase timers, counters, coarse R callback boundaries and the ordinary/native timing gap. It is not an instruction-level attribution of each shader or an additive decomposition of independently timed runs. Sampled native stacks are available for the 1M fixture; other workloads do not have hardware-counter profiles.

| Workload | Main remaining costs indicated by the measurements | Next unimplemented work suggested by that evidence |
|---|---|---|
| small | Frame/depth/output processing; ordinary R calls take substantially longer than native calls | Demand-driven auxiliary buffers and R output assembly |
| grid100k | Geometry/setup/binning and combined coverage/depth/shading, plus R output | Once-per-primitive setup and contiguous bins |
| grid500k | Geometry and attribute allocation/teardown; indexed transforms have mixed end-to-end results | Packed setup/binning before enabling more transform caches |
| grid1m | Geometry access/setup, coverage and clearing; native sampling supports these categories | Geometry layout and setup reuse; keep opt-in transforms under review |
| occluded | Combined coverage/depth/shading with many early-depth failures | Proven opacity classes and a measured opaque visibility path |
| alpha4 | Fragment insertion, coverage/shading, resolve and teardown | Exact sparse transparency with all producers |
| alpha16 | Fragment allocation/resolve/teardown grow with layer count | Exact sparse transparency; no layer caps |
| alpha64 | Tree storage, resolve and teardown dominate; single-render RSS exceeds 2 GiB | Sparse storage with explicit equal-depth ordering and growth |
| ssao | SSAO at one worker; output/resize also matters at high FSAA | Further pass/output work, while retaining scalar parity and sample count |
| shadow | Combined main shading includes unchanged PCF; R/output cost and timing variance remain significant | Separately measure exact depth-only/interior PCF specialization |
| toon | Outline propagation/composition plus R output; serial results vary | Final composition/output passes and better controlled timing |
| shared_textures | Repeated decode count is reduced; geometry/output and shadows remain | Prepared immutable assets only with explicit lifetime/invalidation |
