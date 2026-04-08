#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
demo_dir="$repo_root/demo"
log_file="$(mktemp /tmp/check_test_debug_ast.XXXXXX.log)"

exec > >(tee "$log_file") 2>&1

echo "Logging output to $log_file"

(
  cd "$repo_root"
  dune build ./tools/generate_artifacts.exe ./tools/dump_impl_ast.exe >/dev/null
)

echo "Rebuilding and running demo/test_debug.ml..."
set +e
(
  cd "$demo_dir"
  rm -f generated_ast.txt generated_ml.ml generated_ml_parsed.txt \
    generated_ast.normalized.txt generated_ml_parsed.normalized.txt
  "$repo_root/_build/default/tools/generate_artifacts.exe" \
    --output-dir "$demo_dir" \
    "$demo_dir/test_debug.ml"
  dune build --force --sandbox none ./test_debug.exe
  dune exec ./test_debug.exe
)
build_status=$?
set -e

if [[ $build_status -ne 0 ]]; then
  echo
  echo "Build or execution failed with status $build_status."
  echo "Comparing generated AST dumps anyway..."
fi

echo
echo "Comparing generated AST dumps..."
if "$repo_root/tools/compare_generated_ast.sh" "$demo_dir"; then
  compare_status=0
else
  compare_status=$?
fi

if [[ $build_status -eq 0 && $compare_status -eq 0 ]]; then
  echo
  echo "AST comparison succeeded."
  exit 0
fi

exit 1
