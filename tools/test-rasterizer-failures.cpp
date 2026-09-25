// Standalone fault injection; no allocator hook is linked into the R package.
#include <atomic>
#include <cstdlib>
#include <new>
#include <thread>
#include <iostream>
#include "../src/triangle_setup.h"
#include "../src/fragment_arena.h"

static std::atomic<long> fail_after{-1}, live_allocations{0};
void* operator new(std::size_t bytes) {
  const long remaining=fail_after.load();
  if(remaining>=0 && fail_after.fetch_sub(1)==0) throw std::bad_alloc();
  void* memory=std::malloc(bytes ? bytes : 1);
  if(!memory) throw std::bad_alloc();
  ++live_allocations;
  return memory;
}
void operator delete(void* memory) noexcept {
  if(memory) { --live_allocations; std::free(memory); }
}
void* operator new[](std::size_t bytes) { return ::operator new(bytes); }
void operator delete[](void* memory) noexcept { ::operator delete(memory); }
void operator delete(void* memory,std::size_t) noexcept { ::operator delete(memory); }
void operator delete[](void* memory,std::size_t) noexcept { ::operator delete(memory); }

struct InlinePool {
  template<class F> void parallelFor(int begin,int end,F task,std::size_t) {
    for(int i=begin;i<end;++i) task(i);
  }
  void wait() {}
  void join() {}
};

// The accepted task deliberately waits until join: if the production catch
// fails to join before destroying its scratch arrays, this test fails (and
// ASan catches the dangling access). Test both count and fill submission.
struct PartialPool {
  std::thread worker;
  std::atomic<bool> released{false};
  int calls=0, fail_call, completed=0, joins=0;
  explicit PartialPool(int call):fail_call(call) {}
  template<class F> void parallelFor(int begin,int end,F task,std::size_t) {
    if(++calls!=fail_call) {
      for(int i=begin;i<end;++i) task(i);
      return;
    }
    worker=std::thread([this,task,begin] {
      while(!released.load()) std::this_thread::yield();
      task(begin);
      ++completed;
    });
    throw std::bad_alloc();
  }
  void wait() {}
  void join() { ++joins; released=true; if(worker.joinable()) worker.join(); }
  ~PartialPool() { if(worker.joinable()) { join(); std::abort(); } }
};

template<class Operation> void enumerate_failures(const char* name,Operation operation) {
  int failures=0;
  for(long index=0;index<10000;++index) {
    const long baseline=live_allocations;
    bool failed=false;
    fail_after=index;
    try { operation(); } catch(const std::bad_alloc&) { failed=true; ++failures; }
    fail_after=-1;
    // Partial objects are discarded on error, as in the public render unwind.
    assert(live_allocations==baseline);
    if(!failed) {
      assert(failures>0);
      std::cout << name << ": " << failures << " allocation failures, balanced ownership, recovery passed\n";
      return;
    }
  }
  std::abort();
}

int main() {
  unsetenv("RAYVERTEX_REFERENCE_TRANSPARENCY");
  enumerate_failures("triangle construction and serial bins",[] {
    TriangleBins bins(19,13,4);
    for(int i=0;i<129;++i)
      bins.add_clipped({vec4(-1,-1,0,1),vec4(17,0,0,1),vec4(1,12,0,1)},i,0,3,false);
    bins.build();
    assert(bins.references()>0);
  });
  TriangleBins source(2,2,1);
  for(int i=0;i<65539;++i)
    source.add_clipped({vec4(0,0,0,1),vec4(2,0,0,1),vec4(0,2,0,1)},i,0,3,false);
  enumerate_failures("parallel bin scratch and references",[&] {
    auto bins=source;
    InlinePool pool;
    bins.build_parallel(pool,4);
    assert(bins.build_workers==4);
  });
  for(int pass:{1,2}) {
    auto bins=source;
    PartialPool pool(pass);
    bool failed=false;
    try { bins.build_parallel(pool,4); } catch(const std::bad_alloc&) { failed=true; }
    assert(failed && pool.joins==1 && pool.completed==1 && !pool.worker.joinable());
    InlinePool recovered;
    bins.build_parallel(recovered,4);
    assert(bins.references()>0);
  }
  std::cout << "partial count/fill submission: joined before scratch destruction; subsequent build passed\n";
  for(unsigned mask=0;mask<8;++mask) {
    enumerate_failures("fragment and auxiliary growth",[&] {
      FragmentArena arena(7,5,4,mask);
      for(int layer=0;layer<257;++layer)
        arena.insert(3,2,layer/257.,{vec4(.25),vec3(1),vec3(2),vec3(3)});
      int count=0;
      arena.resolve([&](int,int,Float,const alpha_info&) { ++count; });
      assert(count==257);
    });
  }
}
