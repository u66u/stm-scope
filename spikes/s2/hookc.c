#include "Rts.h"
#include <stdio.h>
#include <stdatomic.h>
static _Atomic int printed;
static const char *where(const StgInfoTable *info) {
  static __thread char b[256];
#if __GLASGOW_HASKELL__ >= 912
  InfoProvEnt ipe; if (!lookupIPE(info, &ipe)) return "?"; InfoProv *p = &ipe.prov;
#else
  InfoProvEnt *e = lookupIPE(info); if (!e) return "?"; InfoProv *p = &e->prov;
#endif
  snprintf(b, sizeof b, "%s %s:%s", p->label, p->src_file, p->src_span); return b;
}
void stmscope_on_atomically(Capability *cap, StgTSO *tso, StgClosure *stm, StgPtr sp) {
  if (atomic_fetch_add(&printed, 1) >= 3) return;
  fprintf(stderr, "site: %s\n", where(UNTAG_CLOSURE(stm)->header.info));
  StgPtr p = sp;
  for (int i = 0; i < 6; i++) {
    const StgRetInfoTable *ri = get_ret_itbl((StgClosure *)p);
    fprintf(stderr, "  frame %d type=%u %s\n", i, ri->i.type, where(((StgClosure *)p)->header.info));
    if (ri->i.type == STOP_FRAME || ri->i.type == UNDERFLOW_FRAME) break;
    p += stack_frame_sizeW((StgClosure *)p);
  }
}
