#include "Rts.h"
#include <stdio.h>
#include <stdatomic.h>

StgBool __real_stmCommitTransaction(Capability *cap, StgTRecHeader *trec);
static _Atomic unsigned long ok_n, fail_n;

StgBool __wrap_stmCommitTransaction(Capability *cap, StgTRecHeader *trec) {
  StgBool r = __real_stmCommitTransaction(cap, trec);
  atomic_fetch_add(r ? &ok_n : &fail_n, 1);
  return r;
}
__attribute__((destructor)) static void report(void) {
  fprintf(stderr, "s1: commits ok=%lu fail=%lu\n", ok_n, fail_n);
}
