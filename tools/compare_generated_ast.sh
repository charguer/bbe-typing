#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
target_dir="${1:-$repo_root/demo}"

generated_ast="$target_dir/generated_ast.txt"
generated_ml="$target_dir/generated_ml.ml"
parsed_ast="$target_dir/generated_ml_parsed.txt"
normalized_generated_ast="$target_dir/generated_ast.normalized.txt"
normalized_parsed_ast="$target_dir/generated_ml_parsed.normalized.txt"

if [[ ! -f "$generated_ast" ]]; then
  echo "Missing file: $generated_ast" >&2
  exit 1
fi

if [[ ! -f "$generated_ml" ]]; then
  echo "Missing file: $generated_ml" >&2
  exit 1
fi

(
  cd "$repo_root"
  dune build ./tools/dump_impl_ast.exe >/dev/null
)

"$repo_root/_build/default/tools/dump_impl_ast.exe" \
  --drop-first 3 \
  "$generated_ml" > "$parsed_ast"

normalize_locations() {
  sed -E \
    -e 's#[A-Za-z0-9_./-]+\[[0-9]+,[0-9]+\+[0-9]+\]\.\.\[[0-9]+,[0-9]+\+[0-9]+\]##g' \
    -e 's# \(\)# #g'
}

normalize_locations < "$generated_ast" > "$normalized_generated_ast"
normalize_locations < "$parsed_ast" > "$normalized_parsed_ast"

echo "Wrote:"
echo "  $parsed_ast"
echo "  $normalized_generated_ast"
echo "  $normalized_parsed_ast"
echo
echo "Comparing normalized dumps:"

if diff -u "$normalized_generated_ast" "$normalized_parsed_ast"; then
  echo
  echo "Parsetrees match after dropping the 3 synthetic prelude items and normalizing locations."
else
  echo
  echo "Parsetrees differ. Raw parsed dump is in $parsed_ast" >&2
  echo "Normalized files:" >&2
  echo "  $normalized_generated_ast" >&2
  echo "  $normalized_parsed_ast" >&2
  echo >&2
  if command -v meld >/dev/null 2>&1; then
    echo "Opening meld..." >&2
    meld "$normalized_generated_ast" "$normalized_parsed_ast" >/dev/null 2>&1 &
  else
    echo "meld is not installed or not in PATH." >&2
  fi
  exit 1
fi
