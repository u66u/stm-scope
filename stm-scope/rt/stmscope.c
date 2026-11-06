/*
 * stm-scope runtime: a link-time interposer on GHC's STM implementation.
 *
 * Linked into a profiled executable with
 *
 *   -Wl,--wrap=stg_atomicallyzh   -Wl,--wrap=stmStartTransaction
 *   -Wl,--wrap=stmCommitTransaction -Wl,--wrap=stmAbortTransaction
 *   -Wl,--wrap=stmWait -Wl,--wrap=stmReWait
 *   -Wl,--wrap=gcStableNameTable -Wl,--wrap=exitScheduler
 *
 * Every hook runs inside an unsafe foreign call with a capability held. Nothing
 * here may allocate on the Haskell heap, block, or trigger a GC; C malloc and
 * short spinlocks are fine. Results leave the process as eventlog user binary
 * messages (wire format below, decoded by StmScope.Event).
 *
 * Facts about rts/STM.c (GHC 9.6 .. 9.14) this file relies on:
 *
 *  F1  stmCommitTransaction frees its TRec by relinking chunk and header
 *      pointers only. Entry contents survive until the next TRec allocation on
 *      the same capability, which cannot happen before the hook returns. The
 *      chunk chain is captured before the call; entries are read after it.
 *  F2  A TVar whose current_value is a TREC_HEADER is locked by a committer.
 *  F3  In-flight invalidation condemns the TRec and lets it fail at commit, so
 *      every conflict restart passes through stmCommitTransaction or stmWait.
 *  F4  gcStableNameTable runs once per GC, single threaded, after all live data
 *      has been evacuated: isAlive() maps an old address to the new one, or to
 *      NULL if the object died. All mutators are stopped.
 *  F5  STM.h: it is always safe to fail a commit that would have succeeded.
 *      Escalation relies on this and on nothing else.
 *
 * Wire format: little endian, every message starts with 'S', a tag byte, then
 *
 *  'D' site    u32 site, u8 n, u64 frame[n]       frame[0] = transaction body
 *  'I' info    u64 info, u16 ptrs, u16 nptrs, 7 x NUL-terminated: con, table,
 *              type, label, module, file, span   (ptrs, nptrs, con: constructors only)
 *  'V' watch   u32 watch, u8 reason, u64 value_info
 *  'S' stats   u32 site, 8 x u32 counters, 3 x u64 sums, u16 n, n x bucket
 *  'G' edge    u32 victim, u32 watch, u32 writer, u8 outcome, 5 x u32 (fails,
 *              fields, con, opaque, locked), u64 wasted, u64 mask, u64 old_info
 *  'N' writes  u32 watch, u64 commits that wrote it   (when it dies, or at exit)
 *  'E' escal   u64 tid, u32 site, u8 level, u32 protected
 *  'R' release u64 tid, u8 reason
 *  'Z' summary u32 version, u32 depth, u32 escalate, 6 x u64 diagnostics
 *
 *  bucket      u16 bucket, 3 x u32 (calls, retried, overlapped a GC),
 *              2 x u64 (wasted, blocked)       one per non-empty latency bucket
 *
 * Volume is O(sites + edges) per flush, independent of the commit rate: all
 * joins happen here, where the facts are, not in the eventlog.
 *
 * Attribution: a commit that writes a watched TVar first records its site in
 * the TVar's watch slot ("intent"), then commits. A victim that later finds
 * the TVar changed or locked reads the slot. If an intent is recorded but the
 * commit then fails, a rival writer of the same TVar is named instead; both
 * contend on it, so the edge is still real.
 */
#include "Rts.h"
/* Only the Capability/StgRegTable offsets are used; these three collide with Block.h. */
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define WIRE_VERSION 4

/* RTS internals: exported from libHSrts, absent from the public headers. */
StgClosure *isAlive(StgClosure *p);
void traceUserBinaryMsg(Capability *cap, uint8_t *msg, size_t size);

/* Originals, resolved by ld --wrap. */
StgTRecHeader *__real_stmStartTransaction(Capability *, StgTRecHeader *);
StgBool __real_stmCommitTransaction(Capability *, StgTRecHeader *);
void __real_stmAbortTransaction(Capability *, StgTRecHeader *);
StgBool __real_stmWait(Capability *, StgTSO *, StgTRecHeader *);
StgBool __real_stmReWait(Capability *, StgTSO *);
void __real_gcStableNameTable(void);
void __real_exitScheduler(bool wait_foreign);

/* Not wrapped: escalation calls the original directly. */
void stmCondemnTransaction(Capability *, StgTRecHeader *);

#if !defined(NO_TREC)
#define NO_TREC ((StgTRecHeader *)(void *)&stg_NO_TREC_closure)
#endif
#define END_CHUNKS ((StgTRecChunk *)(void *)&stg_END_STM_CHUNK_LIST_closure)

/* ---------------------------------------------------------------- basics -- */

enum { OUT_COMMIT, OUT_CONFLICT, OUT_INFLIGHT, OUT_LOCKED, OUT_UNEXPLAINED,
       OUT_RETRY_INVALID, OUT_VETOED, OUT_EXCEPTION };
enum { KIND_VALUE, KIND_LOCKED };
enum { DIFF_FIELDS, DIFF_CON, DIFF_UNKNOWN };
enum { WATCH_CULPRIT, WATCH_UNEXPLAINED };
enum { REL_COMMIT, REL_BLOCKED, REL_ABORT, REL_WATCHDOG, REL_DIED };

#define MAX_DEPTH     6
#define MAX_CAPS      256
#define SITE_CAP      4096            /* power of two; site 0 = unknown */
#define SHARDS        64
#define SHARD_SLOTS   512
#define NBUCKETS      496             /* log-linear, 8 per octave, up to 2^63 ns */
#define EDGE_SLOTS    4096            /* per capability, power of two */
#define ABA_SCAN      64
#define WATCH_INIT    (1u << 14)      /* slots; 32 bytes each */
#define WATCH_MAX     (1u << 20)
#define MSG_MAX       8192
#define PROT_MAX      4096
#define TID_TOMB      (~(StgWord64)0)

static struct {
    unsigned depth;                   /* caller frames per site               */
    unsigned escalate;                /* conflicts before escalation; 0 = off */
    uint64_t slow_ns;                 /* calls slower than this get a 'C'     */
    uint64_t flush_ns;
    uint64_t watchdog_ns;
    bool canary;                      /* verify F1 on every commit            */
} cfg = { 3, 0, 1000000, 200000000, 50000000, false };

static StgTVar **prot;                /* escalation's protected set, see below */
static _Atomic uint64_t dropped_tx, dropped_sites, dropped_watch, dropped_edges, canary_checked, canary_bad;
static uint64_t gc_epoch;              /* GCs so far; written only inside GC */

static uint64_t env_u64(const char *k, uint64_t def)
{
    const char *v = getenv(k);
    return v && *v ? strtoull(v, NULL, 10) : def;
}

__attribute__((constructor)) static void configure(void)
{
    cfg.depth       = (unsigned)env_u64("STMSCOPE_DEPTH", cfg.depth);
    cfg.escalate    = (unsigned)env_u64("STMSCOPE_ESCALATE", 0);
    cfg.slow_ns     = env_u64("STMSCOPE_SLOW_NS", cfg.slow_ns);
    cfg.watchdog_ns = env_u64("STMSCOPE_WATCHDOG_NS", cfg.watchdog_ns);
    cfg.canary      = env_u64("STMSCOPE_CANARY", 0) != 0;
    if (cfg.depth > MAX_DEPTH) cfg.depth = MAX_DEPTH;
    if (cfg.escalate && (prot = calloc(2 * PROT_MAX, sizeof *prot)) == NULL) cfg.escalate = 0;
}

static inline uint64_t now_ns(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (uint64_t)t.tv_sec * 1000000000u + (uint64_t)t.tv_nsec;
}

static inline void relax(void)
{
#if defined(__x86_64__) || defined(__i386__)
    __builtin_ia32_pause();
#elif defined(__aarch64__)
    __asm__ __volatile__("yield");
#endif
}

static inline void spin_lock(atomic_flag *f)
{
    while (atomic_flag_test_and_set_explicit(f, memory_order_acquire)) relax();
}

static inline void spin_unlock(atomic_flag *f)
{
    atomic_flag_clear_explicit(f, memory_order_release);
}

static inline uint64_t mix(uint64_t x)   /* splitmix64 finaliser */
{
    x ^= x >> 30; x *= 0xbf58476d1ce4e5b9ull;
    x ^= x >> 27; x *= 0x94d049bb133111ebull;
    return x ^ (x >> 31);
}

/* Capability is private to the RTS; DerivedConstants.h gives us the offsets. */
static inline uint32_t cap_no(Capability *cap)
{
    return *(uint32_t *)((char *)cap + OFFSET_Capability_no);
}

static inline StgTSO *cur_tso(Capability *cap)
{
    return *(StgTSO **)((char *)cap + OFFSET_Capability_r + OFFSET_StgRegTable_rCurrentTSO);
}

static inline bool is_locked(StgClosure *v)
{
    return GET_INFO(UNTAG_CLOSURE(v)) == &stg_TREC_HEADER_info;
}

/* Log-linear histogram bucket: exact below 16 ns, then 8 sub-buckets per octave. */
static inline unsigned bucket(uint64_t ns)
{
    if (ns < 16) return (unsigned)ns;
    unsigned e = 63u - (unsigned)__builtin_clzll(ns);
    return (e - 2) * 8 + (unsigned)((ns >> (e - 3)) & 7);
}

/* --------------------------------------------------------------- messages -- */

typedef struct { uint8_t *p, *end; } Out;

static inline void put(Out *o, const void *v, size_t n)
{
    if ((size_t)(o->end - o->p) < n) { o->p = o->end + 1; return; }  /* poison */
    memcpy(o->p, v, n);
    o->p += n;
}
static inline void w8 (Out *o, uint8_t  v) { put(o, &v, 1); }
static inline void w16(Out *o, uint16_t v) { put(o, &v, 2); }
static inline void w32(Out *o, uint32_t v) { put(o, &v, 4); }
static inline void w64(Out *o, uint64_t v) { put(o, &v, 8); }
static inline void wstr(Out *o, const char *s) { s = s ? s : ""; put(o, s, strlen(s) + 1); }

/* ---------------------------------------------------------- per capability -- */

/* Commit latency histogram whose buckets also decompose their calls, so the
 * tail can be explained without per-call events. */
typedef struct {
    uint32_t calls, retried, gc;
    uint64_t wasted, blocked;
} Bucket;

typedef struct {
    uint32_t commits, fails, waits, wakes, spurious, aborts, vetoed, conflicts;
    uint64_t latency_sum, blocked_sum, wasted_sum;
    Bucket hist[NBUCKETS];
} SiteStat;

/* Failed attempts by (victim site, culprit TVar, writer site, outcome). */
typedef struct {
    uint32_t victim, watch, writer;
    uint8_t outcome;
    uint32_t fails, fields, con, opaque, locked;   /* fails == 0: empty slot */
    uint64_t wasted, mask, old_info;
} EdgeAgg;

typedef struct {
    Capability *cap;
    uint64_t last_flush;
    /* chunk chain captured before a commit (F1) */
    StgTRecChunk **chunk;
    uint32_t *fill, nchunk, chunk_cap;
    /* canary snapshot */
    TRecEntry *snap;
    size_t nsnap, snap_cap;
    /* stats, sparse over sites */
    SiteStat *stat[SITE_CAP];
    uint32_t touched[SITE_CAP], ntouched;
    EdgeAgg *edge;                    /* EDGE_SLOTS, allocated on first failure */
    uint32_t nedge;
    uint8_t msg[MSG_MAX];
} PerCap;

static PerCap *percap[MAX_CAPS];

static PerCap *pc_of(Capability *cap)
{
    uint32_t n = cap_no(cap);
    if (n >= MAX_CAPS) return NULL;
    PerCap *pc = percap[n];
    if (pc == NULL) {             /* only this capability's owner gets here */
        pc = calloc(1, sizeof *pc);
        if (pc == NULL) return NULL;
        pc->cap = cap;
        percap[n] = pc;
    }
    return pc;
}

static inline Out msg_begin(PerCap *pc, char tag)
{
    Out o = { pc->msg, pc->msg + MSG_MAX };
    w8(&o, 'S');
    w8(&o, (uint8_t)tag);
    return o;
}

static inline void msg_end(PerCap *pc, Out *o)
{
    if (o->p <= o->end) traceUserBinaryMsg(pc->cap, pc->msg, (size_t)(o->p - pc->msg));
}

static SiteStat *stat_of(PerCap *pc, uint32_t site)
{
    SiteStat *s = pc->stat[site];
    if (s == NULL) {
        s = calloc(1, sizeof *s);
        if (s == NULL) return NULL;
        pc->stat[site] = s;
        pc->touched[pc->ntouched++] = site;
    }
    return s;
}

static void flush_stats(PerCap *pc)
{
    for (uint32_t i = 0; i < pc->ntouched; i++) {
        uint32_t site = pc->touched[i];
        SiteStat *s = pc->stat[site];
        if (!(s->commits | s->fails | s->waits | s->wakes | s->spurious | s->aborts)) continue;
        Out o = msg_begin(pc, 'S');
        w32(&o, site);
        w32(&o, s->commits); w32(&o, s->fails); w32(&o, s->waits); w32(&o, s->wakes);
        w32(&o, s->spurious); w32(&o, s->aborts); w32(&o, s->vetoed); w32(&o, s->conflicts);
        w64(&o, s->latency_sum); w64(&o, s->blocked_sum); w64(&o, s->wasted_sum);
        uint16_t n = 0;
        for (unsigned b = 0; b < NBUCKETS; b++) n += s->hist[b].calls != 0;
        w16(&o, n);
        for (unsigned b = 0; b < NBUCKETS; b++) {
            Bucket *k = &s->hist[b];
            if (k->calls == 0) continue;
            w16(&o, (uint16_t)b); w32(&o, k->calls); w32(&o, k->retried); w32(&o, k->gc);
            w64(&o, k->wasted); w64(&o, k->blocked);
        }
        msg_end(pc, &o);
        memset(s, 0, sizeof *s);
    }
    if (pc->nedge != 0) {
        for (uint32_t i = 0; i < EDGE_SLOTS; i++) {
            EdgeAgg *e = &pc->edge[i];
            if (e->fails == 0) continue;
            Out o = msg_begin(pc, 'G');
            w32(&o, e->victim); w32(&o, e->watch); w32(&o, e->writer); w8(&o, e->outcome);
            w32(&o, e->fails); w32(&o, e->fields); w32(&o, e->con); w32(&o, e->opaque); w32(&o, e->locked);
            w64(&o, e->wasted); w64(&o, e->mask); w64(&o, e->old_info);
            msg_end(pc, &o);
        }
        memset(pc->edge, 0, EDGE_SLOTS * sizeof *pc->edge);
        pc->nedge = 0;
    }
}

static EdgeAgg *edge_of(PerCap *pc, uint32_t victim, uint32_t watch, uint32_t writer, uint8_t outcome)
{
    if (pc->edge == NULL && (pc->edge = calloc(EDGE_SLOTS, sizeof *pc->edge)) == NULL) {
        atomic_fetch_add(&dropped_edges, 1);
        return NULL;
    }
    if (pc->nedge >= EDGE_SLOTS - EDGE_SLOTS / 4) flush_stats(pc);   /* full: flush early */
    uint64_t h = mix(((uint64_t)victim << 40) ^ ((uint64_t)writer << 20) ^ watch ^ ((uint64_t)outcome << 60));
    for (uint32_t i = (uint32_t)h & (EDGE_SLOTS - 1);; i = (i + 1) & (EDGE_SLOTS - 1)) {
        EdgeAgg *e = &pc->edge[i];
        if (e->fails == 0) {
            pc->nedge++;
            *e = (EdgeAgg){ .victim = victim, .watch = watch, .writer = writer, .outcome = outcome };
            return e;
        }
        if (e->victim == victim && e->watch == watch && e->writer == writer && e->outcome == outcome) return e;
    }
}

static inline void maybe_flush(PerCap *pc, uint64_t t)
{
    if (t - pc->last_flush >= cfg.flush_ns) {
        pc->last_flush = t;
        flush_stats(pc);
    }
}

/* ------------------------------------------------------- info provenance -- */

/* Info pointers live in static code, so each is resolved and emitted once. */
static _Atomic uintptr_t seen_info[1u << 14];

static void emit_info(PerCap *pc, const StgInfoTable *info)
{
    if (info == NULL) return;
    uintptr_t k = (uintptr_t)info, mask = (1u << 14) - 1;
    for (uintptr_t i = mix(k) & mask, probes = 0; probes <= mask; i = (i + 1) & mask, probes++) {
        uintptr_t cur = atomic_load_explicit(&seen_info[i], memory_order_relaxed);
        if (cur == k) return;
        if (cur == 0) {
            if (!atomic_compare_exchange_strong(&seen_info[i], &cur, k)) {
                if (cur == k) return;
                continue;
            }
            const char *f[7] = { 0 };
            uint16_t ptrs = 0, nptrs = 0;
            /* Constructor names live in the info table itself, so values of types
             * from libraries built without IPE (I#, Bool, ...) are still named. */
            const StgInfoTable *it = INFO_PTR_TO_STRUCT(info);   /* k is the raw header word */
            if (it->type >= CONSTR && it->type <= CONSTR_NOCAF) {
                ptrs  = (uint16_t)it->layout.payload.ptrs;
                nptrs = (uint16_t)it->layout.payload.nptrs;
                f[0] = GET_CON_DESC(itbl_to_con_itbl(it));
            }
#if __GLASGOW_HASKELL__ >= 912
            InfoProvEnt e;
            if (lookupIPE(info, &e)) {
                InfoProv *p = &e.prov;
#else
            InfoProvEnt *ep = lookupIPE(info);
            if (ep != NULL) {
                InfoProv *p = &ep->prov;
#endif
                f[1] = p->table_name; f[2] = p->ty_desc; f[3] = p->label;
                f[4] = p->module; f[5] = p->src_file; f[6] = p->src_span;
            }
            Out o = msg_begin(pc, 'I');
            w64(&o, k);
            w16(&o, ptrs);
            w16(&o, nptrs);
            for (int j = 0; j < 7; j++) wstr(&o, f[j]);
            msg_end(pc, &o);
            return;
        }
    }
}

/* ----------------------------------------------------------------- sites -- */

/* A site is a transaction body plus the continuations it returns to: the same
 * STM action called from two places is two sites. Slots are insert-only. */
typedef struct {
    _Atomic uint64_t key;             /* 0 = empty */
    _Atomic uint8_t ready;
    uint8_t n;
    const StgInfoTable *frame[MAX_DEPTH + 1];
} Site;

static Site sites[SITE_CAP];

static uint32_t site_intern(PerCap *pc, const StgInfoTable **frame, uint8_t n)
{
    uint64_t h = 0x9e3779b97f4a7c15ull ^ n;
    for (uint8_t i = 0; i < n; i++) h = mix(h ^ (uintptr_t)frame[i]);
    h |= 1;
    for (uint32_t i = (uint32_t)(h & (SITE_CAP - 1)), probes = 0; probes < SITE_CAP;
         i = (i + 1) & (SITE_CAP - 1), probes++) {
        if (i == 0) continue;         /* reserved: unknown site */
        Site *s = &sites[i];
        uint64_t k = atomic_load_explicit(&s->key, memory_order_acquire);
        if (k == 0) {
            if (!atomic_compare_exchange_strong(&s->key, &k, h)) {
                if (k != h) continue;
            } else {
                s->n = n;
                memcpy(s->frame, frame, n * sizeof *frame);
                atomic_store_explicit(&s->ready, 1, memory_order_release);
                for (uint8_t j = 0; j < n; j++) emit_info(pc, frame[j]);
                Out o = msg_begin(pc, 'D');
                w32(&o, i);
                w8(&o, n);
                for (uint8_t j = 0; j < n; j++) w64(&o, (uintptr_t)frame[j]);
                msg_end(pc, &o);
                return i;
            }
        }
        if (k != h) continue;
        while (!atomic_load_explicit(&s->ready, memory_order_acquire)) relax();
        if (s->n == n && memcmp(s->frame, frame, n * sizeof *frame) == 0) return i;
    }
    atomic_fetch_add(&dropped_sites, 1);
    return 0;
}

/* --------------------------------------------------- per-transaction state -- */

typedef struct {
    StgWord64 tid;                    /* 0 = empty, TID_TOMB = deleted */
    StgTSO *tso;                      /* weak: fixed up / reaped at GC */
    StgTRecHeader *trec;              /* current top-level TRec, weak */
    uint32_t site, attempts, conflicts;
    bool aborted;                     /* top-level abort seen; may still restart */
    uint64_t t_enter, t_attempt, t_block, blocked;
    uint64_t wasted;                  /* ns in failed attempts of this call */
    uint64_t gc0;                     /* gc_epoch when the call began       */
} Tx;

/* Entries never move between GCs, so a Tx* stays valid without the lock; only
 * the owning thread mutates its own entry. */
static struct { atomic_flag lock; Tx slot[SHARD_SLOTS]; } shard[SHARDS];

static Tx *tx_get(StgWord64 tid, bool create)
{
    __typeof__(shard[0]) *sh = &shard[tid & (SHARDS - 1)];
    Tx *tomb = NULL, *hit = NULL;
    spin_lock(&sh->lock);
    for (uint32_t i = (uint32_t)(mix(tid) & (SHARD_SLOTS - 1)), probes = 0; probes < SHARD_SLOTS;
         i = (i + 1) & (SHARD_SLOTS - 1), probes++) {
        Tx *t = &sh->slot[i];
        if (t->tid == tid) { hit = t; break; }
        if (t->tid == TID_TOMB) { if (tomb == NULL) tomb = t; continue; }
        if (t->tid == 0) { if (tomb == NULL) tomb = t; break; }
    }
    if (hit == NULL && create && tomb != NULL) {
        memset(tomb, 0, sizeof *tomb);
        tomb->tid = tid;
        hit = tomb;
    }
    spin_unlock(&sh->lock);
    if (hit == NULL && create) atomic_fetch_add(&dropped_tx, 1);
    return hit;
}

static void tx_free(Tx *t)
{
    __typeof__(shard[0]) *sh = &shard[t->tid & (SHARDS - 1)];
    spin_lock(&sh->lock);
    t->tid = TID_TOMB;
    spin_unlock(&sh->lock);
}

/* Async exceptions abort a TRec from the thrower's capability: find the owner
 * by TRec. Rare, so a scan is fine. */
static Tx *tx_by_trec(StgTRecHeader *trec)
{
    for (unsigned s = 0; s < SHARDS; s++) {
        Tx *hit = NULL;
        spin_lock(&shard[s].lock);
        for (unsigned i = 0; i < SHARD_SLOTS && hit == NULL; i++) {
            Tx *t = &shard[s].slot[i];
            if (t->tid != 0 && t->tid != TID_TOMB && t->trec == trec) hit = t;
        }
        spin_unlock(&shard[s].lock);
        if (hit) return hit;
    }
    return NULL;
}

/* ---------------------------------------------------------- watched TVars -- */

/* TVars that have been a culprit once. Readers are lock-free, inserts take
 * `wlock`, and the table is rebuilt (and grown) only at GC, when nobody runs. */
typedef struct {
    _Atomic uintptr_t key;
    _Atomic uint32_t id;
    _Atomic uint32_t writer;          /* site of the latest commit to try writing it */
    _Atomic uint64_t when;            /* and when it tried (ns)                       */
    _Atomic uint64_t writes;          /* commits that wrote it                        */
} WSlot;

static WSlot *_Atomic wtab;
static uint32_t wcap, wcount;
static atomic_flag wlock;
static _Atomic uint32_t next_watch = 1;

static WSlot *watch_slot(StgTVar *tv)
{
    WSlot *t = atomic_load_explicit(&wtab, memory_order_acquire);
    if (t == NULL) return NULL;
    uintptr_t k = (uintptr_t)tv, m = wcap - 1;
    for (uintptr_t i = mix(k) & m, probes = 0; probes <= m; i = (i + 1) & m, probes++) {
        uintptr_t cur = atomic_load_explicit(&t[i].key, memory_order_acquire);
        if (cur == k) return &t[i];
        if (cur == 0) return NULL;
    }
    return NULL;
}

static uint32_t watch_find(StgTVar *tv)
{
    WSlot *w = watch_slot(tv);
    return w ? atomic_load_explicit(&w->id, memory_order_relaxed) : 0;
}

/* Only at GC or under wlock: no concurrent inserts. */
static WSlot *wslot_put(WSlot *t, uint32_t cap, uintptr_t k, uint32_t id)
{
    for (uintptr_t i = mix(k) & (cap - 1);; i = (i + 1) & (cap - 1))
        if (atomic_load_explicit(&t[i].key, memory_order_relaxed) == 0) {
            atomic_store_explicit(&t[i].id, id, memory_order_relaxed);
            atomic_store_explicit(&t[i].key, k, memory_order_release);
            return &t[i];
        }
}

static void emit_writes(PerCap *pc, WSlot *w)
{
    Out o = msg_begin(pc, 'N');
    w32(&o, atomic_load_explicit(&w->id, memory_order_relaxed));
    w64(&o, atomic_load_explicit(&w->writes, memory_order_relaxed));
    msg_end(pc, &o);
}

static uint32_t watch(PerCap *pc, StgTVar *tv, uint8_t reason, const StgInfoTable *value_info)
{
    uint32_t id = watch_find(tv);
    if (id != 0) return id;
    spin_lock(&wlock);
    if (wtab == NULL) {
        wcap = WATCH_INIT;   /* grows only at GC, so start roomy */
        atomic_store_explicit(&wtab, calloc(wcap, sizeof(WSlot)), memory_order_release);
    }
    id = watch_find(tv);
    if (id == 0 && wtab != NULL && wcount < wcap - wcap / 4) {
        id = atomic_fetch_add(&next_watch, 1);
        wslot_put(wtab, wcap, (uintptr_t)tv, id);
        wcount++;
    } else if (id == 0) {
        atomic_fetch_add(&dropped_watch, 1);
    }
    spin_unlock(&wlock);
    if (id != 0) {
        emit_info(pc, value_info);
        Out o = msg_begin(pc, 'V');
        w32(&o, id);
        w8(&o, reason);
        w64(&o, (uintptr_t)value_info);
        msg_end(pc, &o);
    }
    return id;
}

/* ------------------------------------------------------------- escalation -- */

static _Atomic StgWord64 privileged;  /* tid holding the token, 0 if none */
static _Atomic int esc_level;         /* 1: protected set, 2: serialise all writers */
static _Atomic int in_commit;
static uint64_t esc_since;
static uint32_t esc_site;
/* prot: open-addressed set of 2 * PROT_MAX slots, built before esc_level is
 * published and rebuilt only at GC. A racing reader can at worst veto one
 * commit spuriously, which F5 allows. */
static _Atomic uint32_t nprot;

static bool prot_has(StgTVar *tv)
{
    uintptr_t m = 2 * PROT_MAX - 1;
    for (uintptr_t i = mix((uintptr_t)tv) & m;; i = (i + 1) & m) {
        if (prot[i] == tv) return true;
        if (prot[i] == NULL) return false;
    }
}

static void prot_add(StgTVar *tv)
{
    uintptr_t m = 2 * PROT_MAX - 1;
    for (uintptr_t i = mix((uintptr_t)tv) & m;; i = (i + 1) & m) {
        if (prot[i] == tv) return;
        if (prot[i] == NULL) { prot[i] = tv; nprot++; return; }
    }
}

static void esc_release(PerCap *pc, StgWord64 tid, uint8_t reason)
{
    StgWord64 expect = tid;
    if (!atomic_compare_exchange_strong(&privileged, &expect, 0)) return;
    atomic_store(&esc_level, 0);
    if (pc != NULL) {
        Out o = msg_begin(pc, 'R');
        w64(&o, tid);
        w8(&o, reason);
        msg_end(pc, &o);
    }
}

/* ------------------------------------------------------- TRec inspection -- */

static void capture(PerCap *pc, StgTRecHeader *trec)
{
    pc->nchunk = 0;
    pc->nsnap = 0;
    for (StgTRecChunk *c = trec->current_chunk;
         c != END_CHUNKS; c = c->prev_chunk) {
        if (pc->nchunk == pc->chunk_cap) {
            uint32_t n = pc->chunk_cap ? 2 * pc->chunk_cap : 64;
            StgTRecChunk **a = realloc(pc->chunk, n * sizeof *a);
            uint32_t *f = realloc(pc->fill, n * sizeof *f);
            if (a) pc->chunk = a;
            if (f) pc->fill = f;
            if (!a || !f) return;
            pc->chunk_cap = n;
        }
        pc->chunk[pc->nchunk] = c;
        pc->fill[pc->nchunk++] = (uint32_t)c->next_entry_idx;
        if (cfg.canary) {
            size_t need = pc->nsnap + c->next_entry_idx;
            if (need > pc->snap_cap) {
                size_t n = need * 2;
                TRecEntry *s = realloc(pc->snap, n * sizeof *s);
                if (!s) continue;
                pc->snap = s;
                pc->snap_cap = n;
            }
            memcpy(pc->snap + pc->nsnap, c->entries, c->next_entry_idx * sizeof(TRecEntry));
            pc->nsnap += c->next_entry_idx;
        }
    }
}

/* The body is variadic so that commas in it need no extra parentheses. */
#define FOR_CAPTURED(pc, e, ...)                                              \
    for (uint32_t _c = 0; _c < (pc)->nchunk; _c++)                            \
        for (uint32_t _i = 0; _i < (pc)->fill[_c]; _i++) {                    \
            TRecEntry *e = &(pc)->chunk[_c]->entries[_i];                     \
            __VA_ARGS__                                                       \
        }

static void canary_check(PerCap *pc)
{
    size_t k = 0;
    bool bad = false;
    FOR_CAPTURED(pc, e, {
        if (k < pc->nsnap) {
            TRecEntry *s = &pc->snap[k];
            bad |= s->tvar != e->tvar || s->expected_value != e->expected_value
                || s->new_value != e->new_value;
        }
        k++;
    });
    atomic_fetch_add(&canary_checked, 1);
    if (bad || k != pc->nsnap) atomic_fetch_add(&canary_bad, 1);
}

/* Follow indirections to a constructor in WHNF; NULL for thunks, functions and
 * anything under evaluation. Never forces anything. */
static StgClosure *whnf(StgClosure *c)
{
    for (int hops = 0; hops < 16; hops++) {
        c = UNTAG_CLOSURE(c);
        const StgInfoTable *in = get_itbl(c);
        switch (in->type) {
        case IND:
        case IND_STATIC:
            c = ((StgInd *)c)->indirectee;
            continue;
        case BLACKHOLE: {
            StgClosure *p = UNTAG_CLOSURE(((StgInd *)c)->indirectee);
            StgHalfWord t = get_itbl(p)->type;
            if (t == TSO || t == BLOCKING_QUEUE) return NULL;
            c = p;
            continue;
        }
        default:
            return in->type >= CONSTR && in->type <= CONSTR_NOCAF ? c : NULL;
        }
    }
    return NULL;
}

typedef struct {
    uint8_t diff;
    const StgInfoTable *old_info, *new_info;
    uint64_t mask;
} Diff;

/* -fdistinct-constructor-tables gives every allocation site of a constructor
 * its own info table, so a record update yields a "different" info pointer.
 * Identity is the tag (kept in srt for constructors), layout and name. */
static bool same_con(const StgInfoTable *a, const StgInfoTable *b)
{
    return a == b
        || (a->type == b->type && a->srt == b->srt
            && a->layout.payload.ptrs == b->layout.payload.ptrs
            && a->layout.payload.nptrs == b->layout.payload.nptrs
            && strcmp(GET_CON_DESC(itbl_to_con_itbl(a)), GET_CON_DESC(itbl_to_con_itbl(b))) == 0);
}

/* Which payload words differ between two values of the same constructor. */
static Diff diff(StgClosure *old, StgClosure *new)
{
    StgClosure *a = whnf(old), *b = whnf(new);
    Diff d = { DIFF_UNKNOWN, a ? a->header.info : NULL, b ? b->header.info : NULL, 0 };
    if (a == NULL || b == NULL) return d;
    const StgInfoTable *in = get_itbl(a);
    if (!same_con(in, get_itbl(b))) { d.diff = DIFF_CON; return d; }
    uint32_t n = in->layout.payload.ptrs + in->layout.payload.nptrs;
    for (uint32_t i = 0; i < n && i < 64; i++)
        if (a->payload[i] != b->payload[i]) d.mask |= 1ull << i;
    d.diff = DIFF_FIELDS;
    return d;
}

static uint32_t slot_writer(WSlot *w)
{
    return w ? atomic_load_explicit(&w->writer, memory_order_acquire) : 0;
}

static void charge(PerCap *pc, uint32_t victim, uint32_t watch, uint32_t writer, uint8_t outcome,
                   uint64_t wasted, uint8_t kind, const Diff *d)
{
    EdgeAgg *e = edge_of(pc, victim, watch, writer, outcome);
    if (e == NULL) return;
    e->fails++;
    e->wasted += wasted;
    if (d == NULL) return;
    if (kind == KIND_LOCKED) e->locked++;
    else if (d->diff == DIFF_FIELDS) e->fields++;
    else if (d->diff == DIFF_CON) e->con++;
    else e->opaque++;
    e->mask |= d->mask;
    if (e->old_info == 0 && d->old_info != NULL) e->old_info = (uintptr_t)d->old_info;
}

/* Explain a failed attempt from the entries in pc's captured chain, and
 * charge its duration, split evenly, to the edges that caused it. */
static uint8_t explain(PerCap *pc, Tx *x, uint64_t dur, uint8_t outcome)
{
    unsigned culprits = 0;
    bool value = false, locked = false;
    FOR_CAPTURED(pc, e, {
        StgClosure *cur = atomic_load_explicit((_Atomic(StgClosure *) *)&e->tvar->current_value,
                                               memory_order_acquire);
        if (is_locked(cur)) locked = true;
        else if (cur != e->expected_value) value = true;
        else continue;
        culprits++;
    });
    if (outcome == OUT_CONFLICT)
        outcome = value ? OUT_CONFLICT : locked ? OUT_LOCKED : OUT_UNEXPLAINED;

    if (culprits != 0) {
        /* TVars keep changing under us: charge at most the culprits counted
         * above, so an attempt is never charged more than its duration. */
        uint64_t share = dur / culprits;
        unsigned charged = 0;
        FOR_CAPTURED(pc, e, {
            if (charged == culprits) break;
            StgClosure *cur = atomic_load_explicit((_Atomic(StgClosure *) *)&e->tvar->current_value,
                                                   memory_order_acquire);
            uint8_t kind = is_locked(cur) ? KIND_LOCKED : KIND_VALUE;
            if (kind == KIND_VALUE && cur == e->expected_value) continue;
            Diff d = diff(e->expected_value, kind == KIND_LOCKED ? e->expected_value : cur);
            if (kind == KIND_LOCKED) d.diff = DIFF_UNKNOWN, d.mask = 0;
            WSlot *w = watch_slot(e->tvar);
            uint32_t id = w ? atomic_load_explicit(&w->id, memory_order_relaxed)
                            : watch(pc, e->tvar, WATCH_CULPRIT, d.old_info);
            emit_info(pc, d.old_info);
            charge(pc, x->site, id, slot_writer(w), outcome, share, kind, &d);
            charged++;
        });
        return outcome;
    }

    /* Nothing differs now. Read-only entries are also checked by update count,
     * so a value changed and changed back (ABA) fails the commit: blame
     * watched read-set TVars written during this attempt, and watch the rest
     * of the read set for next time. */
    uint32_t ids[ABA_SCAN], writers[ABA_SCAN];
    unsigned n = 0, budget = ABA_SCAN;
    if (outcome == OUT_UNEXPLAINED) {
        FOR_CAPTURED(pc, e, {
            if (budget == 0 || e->new_value != e->expected_value) continue;
            budget--;
            WSlot *w = watch_slot(e->tvar);
            if (w == NULL) {
                StgClosure *v = whnf(e->expected_value);
                watch(pc, e->tvar, WATCH_UNEXPLAINED, v ? v->header.info : NULL);
            } else if (atomic_load_explicit(&w->when, memory_order_acquire) >= x->t_attempt) {
                ids[n] = atomic_load_explicit(&w->id, memory_order_relaxed);
                writers[n++] = slot_writer(w);
            }
        });
    }
    for (unsigned i = 0; i < n; i++)
        charge(pc, x->site, ids[i], writers[i], outcome, dur / n, KIND_VALUE, NULL);
    if (n == 0) charge(pc, x->site, 0, 0, outcome, dur, KIND_VALUE, NULL);
    return outcome;
}

/* -------------------------------------------------------------- the hooks -- */

/* Called from hook.cmm on entry to atomically#. */
void stmscope_enter(Capability *cap, StgTSO *tso, StgClosure *stm, StgPtr sp)
{
    if (tso->trec != NO_TREC) return;          /* nested atomically: RTS will throw */
    PerCap *pc = pc_of(cap);
    if (pc == NULL) return;

    const StgInfoTable *frame[MAX_DEPTH + 1];
    uint8_t n = 0;
    StgClosure *body = UNTAG_CLOSURE(stm);
    if (get_itbl(body)->type == PAP) body = UNTAG_CLOSURE(((StgPAP *)body)->fun);
    frame[n++] = body->header.info;
    for (unsigned hops = 0; n <= cfg.depth && hops < 32; hops++) {
        StgClosure *f = (StgClosure *)sp;
        StgHalfWord t = get_ret_itbl(f)->i.type;
        if (t == STOP_FRAME || t == UNDERFLOW_FRAME) break;
        if (t == RET_SMALL || t == RET_BIG) frame[n++] = f->header.info;
        sp += stack_frame_sizeW(f);
    }

    Tx *x = tx_get(tso->id, true);
    if (x == NULL) return;
    x->tso = tso;
    x->trec = NULL;
    x->aborted = false;
    x->site = site_intern(pc, frame, n);
    x->attempts = x->conflicts = 0;
    x->blocked = x->t_block = x->wasted = 0;
    x->gc0 = gc_epoch;
    x->t_enter = now_ns();
}

StgTRecHeader *__wrap_stmStartTransaction(Capability *cap, StgTRecHeader *outer)
{
    StgTRecHeader *trec = __real_stmStartTransaction(cap, outer);
    if (outer == NO_TREC) {
        Tx *x = tx_get(cur_tso(cap)->id, false);
        if (x != NULL) {
            uint64_t t = now_ns();
            if (x->t_block) { x->blocked += t - x->t_block; x->t_block = 0; }
            x->t_attempt = t;
            x->trec = trec;
            x->attempts++;
            x->aborted = false;
        }
    }
    return trec;
}

static bool writes_protected(PerCap *pc)
{
    int level = atomic_load(&esc_level);
    if (level == 0) return false;
    FOR_CAPTURED(pc, e, {
        if (e->new_value != e->expected_value && (level == 2 || prot_has(e->tvar)))
            return true;
    });
    return false;
}

static void maybe_escalate(PerCap *pc, Tx *x, uint64_t t)
{
    if (atomic_load(&privileged) == x->tid) {           /* still failing: serialise */
        if (atomic_load(&esc_level) < 2) {
            atomic_store(&esc_level, 2);
            Out o = msg_begin(pc, 'E');
            w64(&o, x->tid); w32(&o, x->site); w8(&o, 2); w32(&o, nprot);
            msg_end(pc, &o);
        }
        return;
    }
    StgWord64 none = 0;
    if (x->conflicts < cfg.escalate || !atomic_compare_exchange_strong(&privileged, &none, x->tid))
        return;
    memset(prot, 0, 2 * PROT_MAX * sizeof *prot);
    nprot = 0;
    int level = 1;
    FOR_CAPTURED(pc, e, {
        if (nprot < PROT_MAX) prot_add(e->tvar); else level = 2;
    });
    esc_since = t;
    esc_site = x->site;
    atomic_store(&esc_level, level);
    /* Commits that began before the set was published must finish first. */
    while (atomic_load(&in_commit) > 0) relax();
    Out o = msg_begin(pc, 'E');
    w64(&o, x->tid); w32(&o, x->site); w8(&o, (uint8_t)level); w32(&o, nprot);
    msg_end(pc, &o);
}

StgBool __wrap_stmCommitTransaction(Capability *cap, StgTRecHeader *trec)
{
    PerCap *pc = pc_of(cap);
    Tx *x = pc ? tx_get(cur_tso(cap)->id, false) : NULL;
    if (x == NULL) return __real_stmCommitTransaction(cap, trec);

    capture(pc, trec);
    bool condemned = trec->state == TREC_CONDEMNED, vetoed = false;
    if (cfg.escalate) {
        atomic_fetch_add(&in_commit, 1);
        StgWord64 holder = atomic_load(&privileged);
        if (holder != 0 && holder != x->tid) {
            if (now_ns() - esc_since > cfg.watchdog_ns) esc_release(pc, holder, REL_WATCHDOG);
            else if (!condemned && writes_protected(pc)) {
                stmCondemnTransaction(cap, trec);
                vetoed = true;
            }
        }
    }

    /* Intent (see the header): name ourselves on every watched TVar we are
     * about to write, before the commit can make the write visible. */
    if (wcount != 0 && !vetoed) {
        uint64_t t0 = 0;
        FOR_CAPTURED(pc, e, {
            if (e->new_value == e->expected_value) continue;
            WSlot *w = watch_slot(e->tvar);
            if (w == NULL) continue;
            if (t0 == 0) t0 = now_ns();
            atomic_store_explicit(&w->when, t0, memory_order_relaxed);
            atomic_store_explicit(&w->writer, x->site, memory_order_release);
        });
    }

    StgBool ok = __real_stmCommitTransaction(cap, trec);
    uint64_t t = now_ns();
    if (cfg.escalate) atomic_fetch_sub(&in_commit, 1);
    if (cfg.canary) canary_check(pc);

    SiteStat *s = stat_of(pc, x->site);
    if (ok) {
        uint64_t latency = t - x->t_enter - x->blocked;
        if (s) {
            s->commits++;
            s->latency_sum += latency;
            s->blocked_sum += x->blocked;
            Bucket *k = &s->hist[bucket(latency)];
            k->calls++;
            k->retried += x->attempts > 1;
            k->gc += gc_epoch != x->gc0;
            k->wasted += x->wasted;
            k->blocked += x->blocked;
        }
        if (wcount != 0)
            FOR_CAPTURED(pc, e, {
                if (e->new_value == e->expected_value) continue;
                WSlot *w = watch_slot(e->tvar);
                if (w) atomic_fetch_add_explicit(&w->writes, 1, memory_order_relaxed);
            });
        if (atomic_load(&privileged) == x->tid) esc_release(pc, x->tid, REL_COMMIT);
        tx_free(x);
    } else {
        uint64_t dur = t - x->t_attempt;
        uint8_t out = vetoed ? OUT_VETOED : explain(pc, x, dur, condemned ? OUT_INFLIGHT : OUT_CONFLICT);
        x->conflicts++;
        x->wasted += dur;
        if (s) {
            s->fails++;
            s->wasted_sum += dur;
            s->vetoed += out == OUT_VETOED;
            s->conflicts += out != OUT_VETOED;
        }
        if (cfg.escalate && out != OUT_VETOED) maybe_escalate(pc, x, t);
    }
    maybe_flush(pc, t);
    return ok;
}

StgBool __wrap_stmWait(Capability *cap, StgTSO *tso, StgTRecHeader *trec)
{
    StgBool ok = __real_stmWait(cap, tso, trec);
    PerCap *pc = pc_of(cap);
    Tx *x = pc ? tx_get(tso->id, false) : NULL;
    if (x == NULL) return ok;
    uint64_t t = now_ns();
    SiteStat *s = stat_of(pc, x->site);
    if (ok) {                           /* parked on the read set: blocking by design */
        x->t_block = t;
        if (s) s->waits++;
        if (atomic_load(&privileged) == x->tid) esc_release(pc, x->tid, REL_BLOCKED);
    } else {                            /* the read set went stale before we could park */
        capture(pc, trec);              /* TRec was not freed: chain is intact */
        uint64_t dur = t - x->t_attempt;
        explain(pc, x, dur, OUT_RETRY_INVALID);
        x->conflicts++;
        x->wasted += dur;
        if (s) { s->fails++; s->conflicts++; s->wasted_sum += dur; }
    }
    return ok;
}

StgBool __wrap_stmReWait(Capability *cap, StgTSO *tso)
{
    StgBool still = __real_stmReWait(cap, tso);
    PerCap *pc = pc_of(cap);
    Tx *x = pc ? tx_get(tso->id, false) : NULL;
    SiteStat *s = x ? stat_of(pc, x->site) : NULL;
    if (s) { if (still) s->spurious++; else s->wakes++; }
    return still;
}

void __wrap_stmAbortTransaction(Capability *cap, StgTRecHeader *trec)
{
    if (trec->enclosing_trec == NO_TREC) {       /* nested aborts are orElse / catchSTM */
        Tx *x = tx_by_trec(trec);
        PerCap *pc = pc_of(cap);
        if (x != NULL && pc != NULL) {
            SiteStat *s = stat_of(pc, x->site);
            if (s) s->aborts++;
            if (atomic_load(&privileged) == x->tid) esc_release(pc, x->tid, REL_ABORT);
            /* Not freed: a zombie (invalid) transaction that throws is aborted
             * and restarted by the RTS. The entry is recycled by the next
             * atomically on this thread, or reaped at GC when the thread dies. */
            x->aborted = true;
            x->trec = NULL;
        }
    }
    __real_stmAbortTransaction(cap, trec);
}

/* F4: every weak pointer we hold is fixed up here, while the world is stopped. */
void __wrap_gcStableNameTable(void)
{
    __real_gcStableNameTable();
    gc_epoch++;
    PerCap *any = NULL;
    for (unsigned i = 0; i < MAX_CAPS && any == NULL; i++) any = percap[i];

    for (unsigned s = 0; s < SHARDS; s++) {
        Tx live[SHARD_SLOTS];
        unsigned n = 0;
        for (unsigned i = 0; i < SHARD_SLOTS; i++) {
            Tx *t = &shard[s].slot[i];
            if (t->tid == 0 || t->tid == TID_TOMB) continue;
            StgTSO *tso = (StgTSO *)isAlive((StgClosure *)t->tso);
            if (tso == NULL) {                       /* thread died mid-transaction */
                if (atomic_load(&privileged) == t->tid) esc_release(NULL, t->tid, REL_DIED);
                continue;
            }
            live[n] = *t;
            live[n].tso = tso;
            live[n].trec = t->trec ? (StgTRecHeader *)isAlive((StgClosure *)t->trec) : NULL;
            n++;
        }
        memset(shard[s].slot, 0, sizeof shard[s].slot);  /* drops tombstones */
        for (unsigned j = 0; j < n; j++)
            for (uint32_t i = (uint32_t)(mix(live[j].tid) & (SHARD_SLOTS - 1));; i = (i + 1) & (SHARD_SLOTS - 1))
                if (shard[s].slot[i].tid == 0) { shard[s].slot[i] = live[j]; break; }
    }

    WSlot *old = wtab;
    if (old != NULL) {
        uint32_t cap = wcount * 4 >= wcap && wcap < WATCH_MAX ? wcap * 2 : wcap;
        WSlot *fresh = calloc(cap, sizeof(WSlot));
        if (fresh != NULL) {
            uint32_t n = 0;
            for (uint32_t i = 0; i < wcap; i++) {
                uintptr_t k = atomic_load_explicit(&old[i].key, memory_order_relaxed);
                if (k == 0) continue;
                StgClosure *p = isAlive((StgClosure *)k);
                if (p == NULL) {                         /* TVar died: report, forget */
                    if (any != NULL) emit_writes(any, &old[i]);
                    continue;
                }
                WSlot *w = wslot_put(fresh, cap, (uintptr_t)p, atomic_load_explicit(&old[i].id, memory_order_relaxed));
                w->writer = old[i].writer;
                w->when = old[i].when;
                w->writes = old[i].writes;
                n++;
            }
            wtab = fresh;
            wcap = cap;
            wcount = n;
            free(old);
        }
    }

    if (prot != NULL && atomic_load(&privileged) != 0) {
        StgTVar *keep[PROT_MAX];
        uint32_t n = 0;
        for (uint32_t i = 0; i < 2 * PROT_MAX; i++)
            if (prot[i] != NULL) {
                StgTVar *p = (StgTVar *)isAlive((StgClosure *)prot[i]);
                if (p != NULL && n < PROT_MAX) keep[n++] = p;
            }
        memset(prot, 0, 2 * PROT_MAX * sizeof *prot);
        nprot = 0;
        for (uint32_t i = 0; i < n; i++) prot_add(keep[i]);
    }
}

/* Last chance to write: exitScheduler has stopped every Haskell thread, and
 * the capabilities (with their event buffers) are freed only afterwards, by
 * freeScheduler. Each capability's stats go to its own buffer. */
void __wrap_exitScheduler(bool wait_foreign)
{
    __real_exitScheduler(wait_foreign);
    static atomic_flag once;
    if (!atomic_flag_test_and_set(&once)) {
        PerCap *any = NULL;
        for (unsigned i = 0; i < MAX_CAPS; i++)
            if (percap[i] != NULL) { flush_stats(percap[i]); if (any == NULL) any = percap[i]; }
        if (any != NULL) {
            WSlot *t = wtab;
            for (uint32_t i = 0; t != NULL && i < wcap; i++)
                if (t[i].key != 0) emit_writes(any, &t[i]);
            Out o = msg_begin(any, 'Z');
            w32(&o, WIRE_VERSION); w32(&o, cfg.depth); w32(&o, cfg.escalate);
            w64(&o, dropped_tx); w64(&o, dropped_sites); w64(&o, dropped_watch); w64(&o, dropped_edges);
            w64(&o, canary_checked); w64(&o, canary_bad);
            msg_end(any, &o);
        }
    }
}
