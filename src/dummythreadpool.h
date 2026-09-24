#ifndef DUMMYTHREADPOOLH
#define DUMMYTHREADPOOLH

#include <functional>
#include <cstddef>
#include <utility>

class DummyThreadPool {
public:
  DummyThreadPool(int nthreads = 1) { }

  // The push method: run tasks right away in the calling thread
  template <class F, class... Args>
  void push(F&& f, Args&&... args) {
    std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
  }

  // The join method: do nothing, since we ran everything immediately
  void join() { }
  void wait() { }
  template<class F>
  void parallelFor(int begin, int end, F f, std::size_t = 0) {
    for (int i = begin; i < end; ++i) f(i);
  }
};

#endif
