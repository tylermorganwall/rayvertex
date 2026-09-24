# Phase measurements

All times are milliseconds. Warm/native columns show median / sample p95. RSS is full benchmark-process high water (MiB), not per-frame memory; R allocations are total allocated MiB per public call. See the measurement contract in [README.md](README.md). Values below use the retained raw samples, without profiler overhead in public/native wall timings.

## Phase 1 — correctness

Before: `phase0`; after: `corrected`. Full phase median/p95/counter tables: [phase0](phase0/phases.csv), [corrected](corrected/phases.csv). Cold time, FPS and exact sample counts: [phase0](phase0/summary.csv), [corrected](corrected/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| alpha16 | 1 | 5/5 | 1163.00/1176.00 → 1165.00/1179.80 | 1005.00/1014.00 → 973.00/997.40 | 1586.2 → 1661.4 | 525.2 → 534.9 |
| grid100k | 1 | 5/5 | 402.00/409.80 → 426.00/429.20 | 225.00/229.00 → 232.00/234.00 | 1173.6 → 1028.4 | 539.7 → 549.5 |
| grid1m | 1 | 3/5 | 1635.00/1645.80 → 1655.00/1687.60 | 1450.00/1459.00 → 1464.00/1493.80 | 2155.6 → 2263.3 | 670.6 → 680.4 |
| grid500k | 1 | 3/5 | 1005.00/1033.80 → 967.00/1013.80 | 786.00/788.70 → 780.00/873.00 | 1533.0 → 1807.5 | 597.7 → 607.5 |
| occluded | 1 | 5/5 | 490.00/491.60 → 483.00/498.60 | 334.00/337.60 → 303.00/304.00 | 1146.6 → 1182.7 | 539.9 → 549.6 |
| shadow | 1 | 5/5 | 290.00/314.40 → 294.00/298.00 | 126.00/139.20 → 116.00/137.00 | 919.3 → 982.2 | 525.8 → 535.6 |
| small | 1 | 10/5 | 246.00/256.85 → 246.00/258.00 | 69.00/82.25 → 73.00/118.80 | 1097.3 → 866.7 | 527.4 → 537.2 |
| ssao | 1 | 5/5 | 496.00/505.60 → 499.00/502.60 | 318.00/318.00 → 311.00/351.40 | 1032.0 → 1069.9 | 545.3 → 560.0 |
| toon | 1 | 5/5 | 339.00/368.40 → 357.00/365.00 | 192.00/200.20 → 180.00/203.40 | 1010.7 → 961.7 | 525.7 → 535.5 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `ssao` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.01 |
| frame_allocate_clear | 11.98/13.69 | 34.43/38.55 |
| lights_shadow_allocate | 0.07/0.08 | 0.07/0.07 |
| shadow_alpha_allocate | 0.23/0.26 | 0.20/0.22 |
| shader_setup_asset_decode | 0.46/0.48 | 0.44/0.44 |
| model_setup | 0.01/0.02 | 0.01/0.02 |
| bin_and_shadow_shader_allocate | 5.27/6.22 | 4.14/5.45 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 2.96/3.33 | 2.81/3.59 |
| main_coverage_depth_shading | 29.03/32.01 | 31.53/34.43 |
| ssao | 230.85/232.37 | 222.36/233.05 |
| lines_and_transparency_resolve | 0.38/0.42 | 0.39/0.44 |
| depth_conversion | 12.60/12.84 | 12.43/32.05 |
| remaining_output_setup | 21.37/21.72 | 21.00/21.22 |
| native_total | 314.95/320.65 | 333.71/335.62 |

## Phase 2 — work removal

Before: `corrected`; after: `phase2`. Full phase median/p95/counter tables: [corrected](corrected/phases.csv), [phase2](phase2/phases.csv). Cold time, FPS and exact sample counts: [corrected](corrected/summary.csv), [phase2](phase2/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| alpha16 | 1 | 5/5 | 1165.00/1179.80 → 1143.00/1158.60 | 973.00/997.40 → 990.00/1087.20 | 1661.4 → 1491.9 | 534.9 → 529.0 |
| grid100k | 1 | 5/5 | 426.00/429.20 → 284.00/285.60 | 232.00/234.00 → 99.00/100.80 | 1028.4 → 986.4 | 549.5 → 543.5 |
| grid1m | 1 | 5/5 | 1655.00/1687.60 → 562.00/612.60 | 1464.00/1493.80 → 383.00/387.40 | 2263.3 → 1881.6 | 680.4 → 674.4 |
| grid500k | 1 | 5/5 | 967.00/1013.80 → 432.00/447.40 | 780.00/873.00 → 237.00/245.80 | 1807.5 → 1359.4 | 607.5 → 601.5 |
| occluded | 1 | 5/5 | 483.00/498.60 → 363.00/383.20 | 303.00/304.00 → 192.00/195.40 | 1182.7 → 1074.1 | 549.6 → 543.7 |
| shadow | 1 | 5/5 | 294.00/298.00 → 273.00/276.40 | 116.00/137.00 → 91.00/113.20 | 982.2 → 862.1 | 535.6 → 532.6 |
| small | 1 | 5/5 | 246.00/258.00 → 218.00/220.00 | 73.00/118.80 → 41.00/60.40 | 866.7 → 959.7 | 537.2 → 531.2 |
| ssao | 1 | 5/5 | 499.00/502.60 → 463.00/468.20 | 311.00/351.40 → 298.00/308.60 | 1069.9 → 960.5 | 560.0 → 554.0 |
| toon | 1 | 5/5 | 357.00/365.00 → 362.00/364.80 | 180.00/203.40 → 175.00/199.60 | 961.7 → 946.3 | 535.5 → 532.0 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `grid1m` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.01 |
| frame_allocate_clear | 7.54/17.63 | 7.60/17.72 |
| lights_shadow_allocate | 0.06/0.07 | 0.01/0.01 |
| shadow_alpha_allocate | 0.20/0.22 | 0.00/0.00 |
| shader_setup_asset_decode | 476.23/477.16 | 41.89/42.67 |
| model_setup | 20.96/21.24 | 20.61/21.57 |
| bin_and_shadow_shader_allocate | 4.27/5.98 | 3.46/5.81 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 182.53/184.95 | 169.81/179.27 |
| main_coverage_depth_shading | 129.89/131.07 | 110.01/114.68 |
| lines_and_transparency_resolve | 0.41/0.43 | 0.42/0.45 |
| depth_conversion | 12.54/12.62 | 12.58/13.04 |
| remaining_output_setup | 23.66/24.16 | 0.01/0.01 |
| native_total | 860.39/869.53 | 369.99/389.54 |

## Phase 3 — scheduling

Before: `phase2`; after: `phase3`. Full phase median/p95/counter tables: [phase2](phase2/phases.csv), [phase3](phase3/phases.csv). Cold time, FPS and exact sample counts: [phase2](phase2/summary.csv), [phase3](phase3/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| alpha16 | 1 | 5/5 | 1143.00/1158.60 → 1135.00/1136.00 | 990.00/1087.20 → 930.00/940.80 | 1491.9 → 1496.0 | 529.0 → 529.0 |
| grid100k | 1 | 5/5 | 284.00/285.60 → 276.00/284.20 | 99.00/100.80 → 96.00/102.60 | 986.4 → 1099.9 | 543.5 → 543.5 |
| grid1m | 1 | 5/5 | 562.00/612.60 → 545.00/574.60 | 383.00/387.40 → 379.00/414.20 | 1881.6 → 1709.2 | 674.4 → 674.4 |
| grid500k | 1 | 5/5 | 432.00/447.40 → 416.00/438.20 | 237.00/245.80 → 223.00/228.00 | 1359.4 → 1468.0 | 601.5 → 601.5 |
| occluded | 1 | 5/5 | 363.00/383.20 → 356.00/374.40 | 192.00/195.40 → 180.00/191.20 | 1074.1 → 1078.6 | 543.7 → 543.7 |
| shadow | 1 | 5/5 | 273.00/276.40 → 266.00/287.80 | 91.00/113.20 → 88.00/121.40 | 862.1 → 874.8 | 532.6 → 532.6 |
| small | 1 | 5/5 | 218.00/220.00 → 192.00/206.20 | 41.00/60.40 → 36.00/58.80 | 959.7 → 1056.0 | 531.2 → 531.2 |
| ssao | 1 | 5/5 | 463.00/468.20 → 455.00/465.00 | 298.00/308.60 → 281.00/287.80 | 960.5 → 966.6 | 554.0 → 554.0 |
| toon | 1 | 5/5 | 362.00/364.80 → 353.00/360.40 | 175.00/199.60 → 167.00/182.40 | 946.3 → 858.5 | 532.0 → 532.0 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `grid100k` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.01 |
| frame_allocate_clear | 11.96/33.58 | 11.56/32.33 |
| lights_shadow_allocate | 0.01/0.01 | 0.01/0.01 |
| shadow_alpha_allocate | 0.00/0.00 | 0.00/0.00 |
| shader_setup_asset_decode | 4.20/4.35 | 4.05/4.76 |
| model_setup | 2.16/2.70 | 2.46/3.13 |
| bin_and_shadow_shader_allocate | 3.04/3.32 | 3.47/5.43 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 21.99/22.58 | 22.48/25.23 |
| main_coverage_depth_shading | 39.29/39.84 | 36.60/39.22 |
| lines_and_transparency_resolve | 0.38/0.41 | 0.42/0.49 |
| depth_conversion | 12.73/36.41 | 13.68/58.93 |
| remaining_output_setup | 0.01/0.02 | 0.01/0.01 |
| native_total | 98.93/122.34 | 105.51/144.24 |

## Phase 5.1 — early depth

Before: `earlyz-phase3`; after: `earlyz-phase5`. Full phase median/p95/counter tables: [earlyz-phase3](earlyz-phase3/phases.csv), [earlyz-phase5](earlyz-phase5/phases.csv). Cold time, FPS and exact sample counts: [earlyz-phase3](earlyz-phase3/summary.csv), [earlyz-phase5](earlyz-phase5/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| occluded | 1 | 15/15 | 356.00/365.80 → 352.00/372.20 | 173.00/179.90 → 169.00/173.60 | 1231.6 → 1285.4 | 543.7 → 543.7 |
| small | 1 | 15/15 | 208.00/228.30 → 202.00/211.60 | 38.00/43.80 → 36.00/39.00 | 1051.0 → 1044.5 | 531.2 → 531.2 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `occluded` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.01 |
| frame_allocate_clear | 12.08/37.50 | 11.85/40.06 |
| lights_shadow_allocate | 0.01/0.01 | 0.01/0.01 |
| shadow_alpha_allocate | 0.00/0.00 | 0.00/0.00 |
| shader_setup_asset_decode | 3.44/3.69 | 3.36/5.30 |
| model_setup | 1.91/11.32 | 2.07/19.21 |
| bin_and_shadow_shader_allocate | 2.07/4.36 | 2.11/4.27 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 30.84/32.05 | 31.12/33.16 |
| main_coverage_depth_shading | 110.50/127.48 | 106.49/115.62 |
| lines_and_transparency_resolve | 0.39/0.46 | 0.39/0.42 |
| depth_conversion | 13.46/38.03 | 12.69/42.43 |
| remaining_output_setup | 0.01/0.02 | 0.01/0.01 |
| native_total | 179.38/212.99 | 172.86/221.77 |

## Phase 6.4 — texture sharing

Before: `assets-before`; after: `assets-after`. Full phase median/p95/counter tables: [assets-before](assets-before/phases.csv), [assets-after](assets-after/phases.csv). Cold time, FPS and exact sample counts: [assets-before](assets-before/summary.csv), [assets-after](assets-after/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| shared_textures | 1 | 10/10 | 300.50/328.25 → 228.50/243.00 | 138.50/158.25 → 68.50/85.65 | 1289.9 → 1041.3 | 532.6 → 532.6 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `shared_textures` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.02 |
| frame_allocate_clear | 12.11/13.41 | 12.68/15.03 |
| lights_shadow_allocate | 0.07/0.08 | 0.07/0.08 |
| shadow_alpha_allocate | 0.21/0.22 | 0.23/0.31 |
| shader_setup_asset_decode | 30.82/33.67 | 2.17/3.15 |
| model_setup | 0.03/0.07 | 0.01/0.02 |
| bin_and_shadow_shader_allocate | 33.21/34.27 | 4.26/6.83 |
| shadow_passes | 2.64/2.70 | 1.72/2.30 |
| main_transform_setup_bins | 1.54/1.74 | 1.60/1.99 |
| main_coverage_depth_shading | 30.64/32.38 | 28.14/31.79 |
| lines_and_transparency_resolve | 0.40/0.49 | 0.38/0.42 |
| depth_conversion | 12.14/12.49 | 12.56/13.60 |
| remaining_output_setup | 0.01/0.02 | 0.01/0.01 |
| native_total | 125.08/129.30 | 65.56/72.25 |

## Phase 8 — parallel screen passes

Before: `screen-phase6`; after: `screen-phase8`. Full phase median/p95/counter tables: [screen-phase6](screen-phase6/phases.csv), [screen-phase8](screen-phase8/phases.csv). Cold time, FPS and exact sample counts: [screen-phase6](screen-phase6/summary.csv), [screen-phase8](screen-phase8/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| ssao | 4 | 10/10 | 443.00/447.65 → 280.50/293.20 | 261.50/265.10 → 106.00/110.00 | 1066.4 → 1021.5 | 554.0 → 554.0 |
| toon | 4 | 10/10 | 331.00/353.70 → 267.00/291.65 | 153.50/155.55 → 88.00/93.10 | 1127.3 → 1050.5 | 532.0 → 532.0 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `ssao` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.02 | 0.01/0.01 |
| frame_allocate_clear | 10.96/36.61 | 10.39/36.94 |
| lights_shadow_allocate | 0.01/0.01 | 0.01/0.01 |
| shadow_alpha_allocate | 0.00/0.00 | 0.00/0.00 |
| shader_setup_asset_decode | 0.06/0.06 | 0.06/0.10 |
| model_setup | 0.01/0.02 | 0.01/0.04 |
| bin_and_shadow_shader_allocate | 3.63/5.95 | 3.68/4.95 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 3.29/3.72 | 3.14/3.37 |
| main_coverage_depth_shading | 6.21/6.76 | 6.24/6.54 |
| ssao | 222.45/249.00 | 66.62/91.13 |
| lines_and_transparency_resolve | 0.38/0.41 | 0.45/0.47 |
| depth_conversion | 12.59/39.82 | 13.36/39.36 |
| remaining_output_setup | 0.01/0.01 | 0.01/0.01 |
| native_total | 285.11/288.43 | 127.08/132.50 |

## Phase 4.1 — experimental transforms

Before: `transforms-phase8`; after: `transforms-indexed`. Full phase median/p95/counter tables: [transforms-phase8](transforms-phase8/phases.csv), [transforms-indexed](transforms-indexed/phases.csv). Cold time, FPS and exact sample counts: [transforms-phase8](transforms-phase8/summary.csv), [transforms-indexed](transforms-indexed/summary.csv).

| Case | Workers | N before/after | Warm before → after | Native before → after | RSS before → after | R allocations before → after |
|---|---:|---:|---|---|---|---|
| grid100k | 1 | 5/5 | 272.00/278.00 → 270.00/274.00 | 93.00/94.80 → 94.00/94.80 | 1080.9 → 1177.8 | 543.5 → 543.5 |
| grid1m | 1 | 5/5 | 549.00/590.80 → 530.00/561.80 | 375.00/415.60 → 356.00/356.80 | 1744.4 → 1732.0 | 674.4 → 674.4 |
| grid500k | 1 | 5/5 | 405.00/423.40 → 435.00/449.40 | 218.00/222.00 → 208.00/216.60 | 1423.8 → 1333.0 | 601.5 → 601.5 |
| occluded | 1 | 5/5 | 352.00/361.60 → 344.00/348.20 | 169.00/171.80 → 161.00/163.80 | 1100.6 → 1165.5 | 543.7 → 543.7 |

All these phase fixtures use 800×800 output, FSAA 1.

Phase diagnostics for `grid1m` (median / p95; instrumented, so not additive to the uninstrumented timings above):

| Native phase | Before | After |
|---|---:|---:|
| environment_decode | 0.01/0.01 | 0.01/0.01 |
| frame_allocate_clear | 7.20/15.56 | 7.40/16.13 |
| lights_shadow_allocate | 0.01/0.01 | 0.01/0.01 |
| shadow_alpha_allocate | 0.00/0.00 | 0.00/0.00 |
| shader_setup_asset_decode | 42.48/42.67 | 40.09/41.84 |
| model_setup | 20.92/23.11 | 21.19/21.59 |
| bin_and_shadow_shader_allocate | 3.68/4.32 | 3.62/5.15 |
| shadow_passes | 0.00/0.00 | 0.00/0.00 |
| main_transform_setup_bins | 168.99/193.02 | 151.70/151.71 |
| main_coverage_depth_shading | 104.55/105.38 | 99.86/103.50 |
| lines_and_transparency_resolve | 0.46/0.63 | 0.45/0.45 |
| depth_conversion | 12.64/12.81 | 11.94/12.15 |
| remaining_output_setup | 0.01/0.01 | 0.01/0.01 |
| native_total | 370.27/385.58 | 343.06/356.16 |
