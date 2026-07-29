#!/usr/bin/env bash
# replay_interactive.sh — Replays the interactive synthesis process for the 4 interactive benchmarks.
#
# For each benchmark, synthesis is run with witness.dl (positive example traces provided
# by the user during the interactive process). Each witness scenario is a transaction
# sequence the synthesized contract must allow; the synthesizer excludes any guard predicate
# that would block a witness step.
#
# Usage:
#   ./scripts/replay_interactive.sh              # all 4 benchmarks
#   ./scripts/replay_interactive.sh nft auction  # specific benchmarks
#
# Output files are written to synthesis-output/<benchmark>_replay.dl

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

export LD_LIBRARY_PATH=lib:$LD_LIBRARY_PATH
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH=$JAVA_HOME/bin:$PATH

BENCHMARKS="${*:-tokenPartition ltcSwapAsset auction nft}"

PASS=0
FAIL=0

for B in $BENCHMARKS; do
  BENCH_DIR="synthesis-benchmark/$B"
  WITNESS="$BENCH_DIR/witness.dl"
  OUTPUT="synthesis-output/${B}_replay.dl"

  echo "========================================"
  echo "Benchmark: $B"
  echo "Witness:   $WITNESS"
  echo "Output:    $OUTPUT"
  echo "========================================"

  if [ ! -d "$BENCH_DIR" ]; then
    echo "ERROR: benchmark directory $BENCH_DIR not found"
    FAIL=$((FAIL+1))
    continue
  fi

  if [ ! -f "$WITNESS" ]; then
    echo "WARNING: no witness.dl found for $B — running without witness traces"
  fi

  # Remove previous replay output so synthesis runs fresh
  rm -f "$OUTPUT"

  echo "[1/2] Running synthesis with witness traces..."
  SBT_OPTS="-Xss4m" sbt -J-Xmx4g \
    "runMain Main synthesis-all $B" \
    2>&1 | grep -E "\[CEGIS\]|\[Witness\]|Synthesis output|^\[" | head -60 || true

  # synthesis-all writes to synthesis-output/<name>.dl, not _replay.dl; copy it
  if [ -f "synthesis-output/${B}.dl" ]; then
    cp "synthesis-output/${B}.dl" "$OUTPUT"
    echo "Synthesis output copied to $OUTPUT"
  else
    echo "ERROR: synthesis-output/${B}.dl not found after synthesis"
    FAIL=$((FAIL+1))
    continue
  fi

  echo "[2/2] Verifying $OUTPUT..."
  VERIFY_RESULT=$(SBT_OPTS="-Xss4m" sbt -J-Xmx4g \
    "runMain Main verify $BENCH_DIR $OUTPUT" \
    2>&1 | tail -10)
  echo "$VERIFY_RESULT"

  if echo "$VERIFY_RESULT" | grep -qi "pass\|success\|verified"; then
    echo "RESULT: PASS"
    PASS=$((PASS+1))
  else
    echo "RESULT: FAIL (check output above)"
    FAIL=$((FAIL+1))
  fi
  echo ""
done

echo "========================================"
echo "Summary: $PASS passed, $FAIL failed"
echo "========================================"
