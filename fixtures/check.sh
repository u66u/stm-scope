#!/usr/bin/env bash
# Build every fixture with stm-scope, run it, and check that the report names
# the pathology the fixture was written to have.
#   usage: fixtures/check.sh [GHC]      (default ghc)
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
ghc=${1:-ghc}
root=$here/..
(cd "$root" && cabal build -v0 -w "$ghc" exe:stm-scope)
ss=$(cd "$root" && cabal list-bin -w "$ghc" exe:stm-scope)
export stm_scope_datadir=$root/stm-scope        # data files from the source tree
cd "$here"
"$ss" build -w "$ghc" -- -v0 all
mkdir -p out
fail=0
exe() { cabal list-bin -w "$ghc" --builddir=dist-stm-scope "$1"; }
ok() { echo "ok    $1"; }
bad() { echo "FAIL  $1  (out/${1%%:*}.txt)"; fail=1; }
clean() { ! grep -q '^!' "out/$1.txt"; }
run() {   # fixture [run options]: report in out/FIXTURE.txt
  local f=$1; shift
  "$ss" run --canary "$@" -o "out/$f.eventlog" "$(exe "$f")" > "out/$f.txt"
}

# The top finding is the pathology.
while read -r f expect; do
  run "$f"
  top=$(grep -m1 '^#1 ' "out/$f.txt" || true)
  if [[ $top == *"$expect"* ]] && clean "$f"; then ok "$f: $expect"; else bad "$f: expected $expect, got: ${top:-nothing}"; fi
done <<'LIST'
false-sharing SPLIT
hot-counter   HOT SCALAR
long-reader   LONG VICTIM
herd          THUNDERING HERD
queue-wait    RETRY BY DESIGN
aba-flag      ABA CHURN
LIST

# Field names come from the build, and the victim is known not to read the field.
if grep -q 'changes {jobs}' out/false-sharing.txt && grep -q 'victim reads {config, sessions}' out/false-sharing.txt \
   && grep -q 'TVar    app ' out/false-sharing.txt; then ok "false-sharing: names the field, the reads and the TVar"
else bad "false-sharing: field, read or TVar names missing"; fi

# orElse, throwSTM and killed threads: nothing lost, nothing misreported.
run orelse-exc
if clean orelse-exc && ! grep -q '^#.*UNATTRIBUTED' out/orelse-exc.txt; then ok "orelse-exc: clean"; else bad "orelse-exc: diagnostics or unattributed cost"; fi

# One STM action, two callers through one helper: two sites.
run helper-site
if grep -q '←  main\.producer' out/helper-site.txt && grep -q '←  main\.consumer' out/helper-site.txt && clean helper-site
then ok "helper-site: both callers told apart"; else bad "helper-site: callers not told apart"; fi

# Escalation: same answer as without it, consistent snapshots, bounded retries.
plain=$("$(exe escalate)")
run escalate --escalate 8
first=$(head -1 out/escalate.txt)
# the reader's rows in the site table (one per stack shape it ran from)
read -r commits fails <<<"$(tr -d , < out/escalate.txt | awk '/^ +[0-9]+ +[0-9]+ .*main\.snapshot/ { c += $1; f += $2 } END { print c + 0, f + 0 }')"
if [[ $first == "$plain" && $first == "snapshots consistent"* ]] && grep -q '^escalation: ' out/escalate.txt \
   && (( ${commits:-0} >= 50 && ${fails:-999999} <= 10 * ${commits:-0} )) && clean escalate; then
  ok "escalate: $first, reader $commits commits / $fails fails"
else bad "escalate: got '$first' (plain '$plain'), reader ${commits:-?} commits / ${fails:-?} fails"; fi

# S3: a TVar keeps its watch across GCs that move it: one unattributed
# failure per TVar at most (its first), however many GCs ran.
run gc-watch
num() { tr -d , < out/gc-watch.txt | awk "$1"; }
gcs=$(sed -n 's/^gcs \([0-9]*\),.*/\1/p' out/gc-watch.txt)
watched=$(num '/watched TVars/ { print $(NF - 2) }')
unwatched=$(num '/before the TVar was watched/ { s += $2 } END { print s + 0 }')
attributed=$(num '/^    writer .*main\.writer/ { s += $2 } END { print s + 0 }')
if (( ${gcs:-0} >= 20 && ${watched:-9999} <= 1000 && unwatched <= watched && attributed >= 2 * unwatched )) && clean gc-watch
then ok "gc-watch: $gcs GCs, $watched TVars watched, $unwatched unattributed, $attributed attributed"
else bad "gc-watch: ${gcs:-?} GCs, ${watched:-?} watched, $unwatched unattributed, $attributed attributed"; fi

# S7: payload layouts derived from .hi files match real heap objects.
line=$(grep -n '^main =' src/Layout.hs | cut -d: -f1)
derived=$(printf 'site\t0\tMain\tsrc/Layout.hs\t%s:1-4\n' "$line" | dist-stm-scope/stm-scope-facts dist-stm-scope \
          | awk -F'\t' '$1 == "con" { print "obs\t" $3 "\t" $7 }' | sort)
observed=$("$(exe layout)" | sort)
echo "$observed" > out/layout.txt
missing=$(comm -23 <(echo "$observed") <(echo "$derived"))
if [[ -z $missing ]]; then ok "layout: $(wc -l <<<"$observed") constructors match the heap"
else echo "$missing" >> out/layout.txt; bad "layout: $(wc -l <<<"$missing") constructors differ"; fi
exit $fail
