#ifndef RAYVERTEX_RASTER_SCHEDULER_H
#define RAYVERTEX_RASTER_SCHEDULER_H

#include <algorithm>
#include <cstdlib>
#include <limits>
#include <stdexcept>
#include <vector>
#include "RcppThread.h"

inline std::size_t raster_batch_size() {
  const char* setting = std::getenv("RAYVERTEX_BATCH_BLOCKS");
  if (!setting || !*setting) return 64;
  char* end = nullptr;
  long value = std::strtol(setting, &end, 10);
  if (*end || value < 1 || value > 65536)
    throw std::invalid_argument("RAYVERTEX_BATCH_BLOCKS must be between 1 and 65536");
  return static_cast<std::size_t>(value);
}

// Work units remain the original coverage blocks. Each sample has one owner;
// batching never changes the primitive sequence within a block.
template<class Pool, class Blocks, class Task>
std::size_t dispatch_raster_blocks(Pool& pool, const Blocks& blocks, Task task,
                                  int workers, std::size_t batch_size, bool reference) {
  if (reference) {
    for (std::size_t i = 0; i < blocks.size(); ++i) pool.push(task, i);
    pool.wait();
    return blocks.size();
  }
  std::vector<std::size_t> active;
  active.reserve(blocks.size());
  for (std::size_t i = 0; i < blocks.size(); ++i) {
    if (blocks.active(i)) active.push_back(i);
  }
  if (active.empty()) return 0;
  if (workers <= 1) {
    for (std::size_t i = 0; i < active.size(); ++i) {
      if (i % 256 == 0) RcppThread::checkUserInterrupt();
      task(active[i]);
    }
    return 0; // no queued tasks in serial mode
  }
  if (active.size() > static_cast<std::size_t>(std::numeric_limits<int>::max()))
    throw std::overflow_error("Too many active raster blocks");
  const std::size_t batches = 1 + (active.size()-1) / batch_size;
  try {
    pool.parallelFor(0, static_cast<int>(active.size()), [&](std::size_t i) {
      task(active[i]);
    }, batches);
    pool.wait(); // active list and task references remain alive through this barrier
  } catch (...) {
    // Also retain the active list if allocation fails partway through submission.
    try { pool.join(); } catch (...) {}
    throw;
  }
  RcppThread::checkUserInterrupt();
  return batches;
}

// Each invocation is a complete screen-pass barrier. The reference override is
// for developer parity/timing checks and does not change the public R API.
template<class Pool, class Task>
void dispatch_screen_rows(Pool& pool, int workers, int count, Task task) {
  if (workers <= 1 || count < 64 || std::getenv("RAYVERTEX_REFERENCE_SCREEN")) {
    for (int i = 0; i < count; ++i) task(i);
    return;
  }
  try {
    pool.parallelFor(0, count, task);
    pool.wait();
  } catch (...) {
    try { pool.join(); } catch (...) {}
    throw;
  }
  RcppThread::checkUserInterrupt();
}
// Tile records and destination samples have the same exclusive ownership.
// No R callbacks or allocations occur in the resolve workers.
template<class Pool, class Arena, class Resolve>
void dispatch_fragment_tiles(Pool& pool,int workers,Arena& arena,Resolve resolve) {
  if(arena.size()>std::size_t(std::numeric_limits<int>::max()))
    throw std::overflow_error("Too many transparency tiles");
  dispatch_screen_rows(pool,workers,int(arena.size()),[&](int tile) {
    arena.resolve_tile(tile,resolve);
  });
}
#endif
