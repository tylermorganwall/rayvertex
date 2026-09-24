# Measured phase: e512c02 → d48a259

800×800; FSAA 1; one worker; unchanged fixture/lighting/quality. Three warm samples; three separate uninstrumented native samples; three diagnostic profiles. Cold is the first render in a fresh R process including preparation/decode, with OS file caches uncontrolled. Fresh-render RSS is a separate one-render process. Sustained mean covers five ordinary calls plus final release/GC; warm/native samples force GC outside each timer.

| Scene | Cold ms | Warm median / p95 ms | Native median / p95 ms | Sustained mean ms | Fresh RSS MiB |
|---|---:|---:|---:|---:|---:|
| alpha16 | 885.0 → 570.0 | 775.0 → 510.0 / 800.2 → 518.1 | 640.0 → 327.0 / 641.8 → 327.9 | 797.8 → 512.2 | 755.6 → 371.9 |
| alpha4 | 463.0 → 339.0 | 329.0 → 267.0 / 354.2 → 273.3 | 182.0 → 86.0 / 186.5 → 87.8 | 346.0 → 272.8 | 453.5 → 375.0 |
| alpha64 | 2364.0 → 1399.0 | 2268.0 → 1335.0 / 2272.5 → 1338.6 | 2110.0 → 1160.0 / 2111.8 → 1161.8 | 2272.6 → 1326.8 | 2041.8 → 1037.0 |
| environment | 367.0 → 309.0 | 226.0 → 246.0 / 239.5 → 246.0 | 72.0 → 50.0 / 73.8 → 51.8 | 237.6 → 242.8 | 458.5 → 377.5 |
| environment_shared | 399.0 → 313.0 | 276.0 → 250.0 / 280.5 → 273.4 | 101.0 → 53.0 / 101.0 → 53.9 | 288.4 → 247.8 | 575.4 → 392.0 |
| grid100k | 360.0 → 290.0 | 242.0 → 214.0 / 248.3 → 229.3 | 69.0 → 37.0 / 70.8 → 37.0 | 244.6 → 204.6 | 503.9 → 433.8 |
| grid1m | 615.0 → 472.0 | 508.0 → 403.0 / 529.6 → 406.6 | 342.0 → 217.0 / 369.9 → 221.5 | 541.6 → 420.4 | 1298.5 → 1119.9 |
| grid500k | 488.0 → 396.0 | 364.0 → 278.0 / 388.3 → 284.3 | 194.0 → 127.0 / 198.5 → 129.7 | 383.2 → 306.2 | 788.1 → 678.2 |
| point_lights | 376.0 → 309.0 | 265.0 → 241.0 / 267.7 → 261.7 | 94.0 → 60.0 / 94.9 → 60.0 | 270.8 → 240.2 | 373.4 → 373.0 |
| shadow | 344.0 → 270.0 | 226.0 → 205.0 / 235.9 → 219.4 | 68.0 → 20.0 / 68.0 → 20.0 | 237.2 → 189.2 | 449.5 → 384.4 |
| shared_textures | 311.0 → 269.0 | 205.0 → 202.0 / 210.4 → 207.4 | 46.0 → 20.0 / 46.9 → 20.9 | 208.8 → 195.8 | 491.2 → 394.2 |
| small | 291.0 → 256.0 | 182.0 → 193.0 / 186.5 → 193.0 | 26.0 → 7.0 / 27.8 → 7.9 | 184.8 → 183.0 | 436.2 → 355.0 |
| ssao | 629.0 → 390.0 | 482.0 → 274.0 / 490.1 → 274.9 | 304.0 → 106.0 / 341.8 → 106.9 | 441.6 → 285.6 | 417.8 → 410.2 |
| toon | 440.0 → 411.0 | 303.0 → 331.0 / 314.7 → 334.6 | 157.0 → 144.0 / 164.2 → 148.5 | 318.8 → 333.2 | 469.1 → 452.8 |

Image/buffer comparisons: 238; exact: 238. Maximum absolute difference: 0. Per-buffer nonfinite, attribute, and changed-sample counts are in `image-differences.csv`.

Phase median/p95 timings and diagnostic counters are in `before-summary/phases.csv` and `after-summary/phases.csv`. Summaries also retain measured R allocations and diagnostic overhead. Raw samples, profiler records, process logs, and peak-RSS records are alongside them. RSS variation includes R/allocator/runtime behavior and fixture construction, not just native payload. Three samples do not establish stable tail latency. Compare full timings and memory; do not infer a general speedup from one stage.
