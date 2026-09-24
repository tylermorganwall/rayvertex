#ifndef RAYVERTEX_RASTER_PROFILE_H
#define RAYVERTEX_RASTER_PROFILE_H

#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <string>
#include <utility>
#include <vector>

// Developer-only instrumentation. No file access or clocks in workers, and no
// public return-value changes. A render emits its records after the last barrier.
class RasterProfile {
  using Clock = std::chrono::steady_clock;
  const char* path;
  Clock::time_point start, last;
  std::vector<std::pair<std::string, double>> records;
  bool completed = false;
public:
  RasterProfile() : path(std::getenv("RAYVERTEX_PROFILE")) {
    if (enabled()) start = last = Clock::now();
  }
  bool enabled() const { return path && *path; }
  void mark(const std::string& phase) {
    if (!enabled()) return;
    auto now = Clock::now();
    records.emplace_back(phase, std::chrono::duration<double, std::milli>(now-last).count());
    last = now;
  }
  void count(const std::string& name, double value) {
    if (enabled()) records.emplace_back("count_" + name, value);
  }
  // The profiler is the first frame local, so its destructor sees buffer and
  // shader teardown as well. Failed renders do not emit completed-frame rows.
  void finish() { completed = true; last = enabled() ? Clock::now() : last; }
  ~RasterProfile() noexcept {
    if (!completed || !enabled()) return;
    try { write_records(); } catch (...) { /* diagnostics must not break a render */ }
  }
private:
  void write_records() {
    if (!enabled()) return;
    auto end = Clock::now();
    records.emplace_back("native_teardown", std::chrono::duration<double, std::milli>(end-last).count());
    const double total = std::chrono::duration<double, std::milli>(end-start).count();
    std::ofstream output(path, std::ios::app);
    output << std::setprecision(12);
    for (const auto& record : records) output << record.first << ',' << record.second << '\n';
    output << "native_total," << total << '\n';
  }
};

struct RasterCounters {
  std::size_t candidates = 0, covered = 0, early_z = 0, shaded = 0, transparent = 0;
};

#endif
