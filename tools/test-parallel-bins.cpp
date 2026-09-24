#include "../src/triangle_setup.h"
#include <thread>
#include <random>
#include <iostream>

struct TestPool {
  template<class F> void parallelFor(int begin,int end,F task,std::size_t) {
    std::vector<std::thread> workers;
    try {
      for(int i=begin;i<end;++i) workers.emplace_back([&,i] { task(i); });
    } catch(...) {
      for(auto& thread:workers) thread.join();
      throw;
    }
    for(auto& thread:workers) thread.join();
  }
  void wait() {}
  void join() {}
};

void compare(const TriangleBins& a,const TriangleBins& b) {
  assert(a.size()==b.size() && a.references()==b.references());
  for(std::size_t tile=0;tile<a.size();++tile) {
    assert(a.begin(tile)==b.begin(tile) && a.end(tile)==b.end(tile));
    for(std::size_t i=a.begin(tile);i<a.end(tile);++i) {
      const auto& x=a.at(i); const auto& y=b.at(i);
      assert(x.face==y.face && x.material==y.material && x.clip_weights==y.clip_weights);
      assert(x.vertices==y.vertices);
    }
  }
}

int main() {
  TestPool pool;
  std::mt19937 random(198);
  std::uniform_real_distribution<double> coord(-8,45);
  TriangleBins reference(37,29,4);
  for(int i=0;i<65539;++i) {
    std::array<vec4,3> triangle;
    for(auto& p:triangle) p=vec4(coord(random),coord(random),0.25+(i%3)*0.1,1);
    reference.add_clipped(triangle,i,i%5,3,false);
  }
  reference.build();
  for(int workers:{1,2,4,10}) {
    auto parallel=reference;
    parallel.build_parallel(pool,workers);
    compare(reference,parallel);
    assert(parallel.build_workers<=workers && parallel.build_scratch_bytes<=32*1024*1024);
  }
  auto bounded=reference;
  bounded.build_parallel(pool,10,reference.size()*sizeof(std::size_t)*2);
  assert(bounded.build_workers==2);
  compare(reference,bounded);
  bounded.build_parallel(pool,10,0);
  assert(bounded.build_workers==1);
  compare(reference,bounded);
  TriangleBins empty(1,1,1);
  empty.build_parallel(pool,4);
  assert(empty.references()==0 && empty.build_workers==1);
  std::cout << "parallel bins: exact offsets and submission order, clipped fans, worker/budget bounds and serial fallback passed\n";
}
