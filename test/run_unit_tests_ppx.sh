#!/bin/bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="${ROOT_DIR}/test"
INPUT_FILE="${TEST_DIR}/unit_tests_ppx.ml"
GENERATED_FILE="${TEST_DIR}/unit_tests_ppx_generated.ml"
DUMP_FILE="${ROOT_DIR}/output.txt"
BUILD_DIR="${TEST_DIR}/_ppx_build"
EXECUTABLE="${BUILD_DIR}/unit_tests_ppx_generated.exe"
BYTECODE_OBJECT="${BUILD_DIR}/unit_tests_ppx_generated.cmo"

mkdir -p "${BUILD_DIR}"
rm -f "${DUMP_FILE}"

set +e
{
  echo "== unit_tests_ppx pipeline =="
  echo "root: ${ROOT_DIR}"
  echo "input: ${INPUT_FILE}"
  echo "generated: ${GENERATED_FILE}"
  echo "date: $(date -Iseconds)"
  echo

  echo "== Step 1: Generate unit_tests_ppx_generated.ml with dune =="
  (
    cd "${ROOT_DIR}"
    dune build test/unit_tests_ppx_generated.ml
  )
  step_status=$?
  if [ "${step_status}" -ne 0 ]; then
    echo "Generation failed with status ${step_status}."
    exit "${step_status}"
  fi
  echo

  echo "== Step 2: OCaml typecheck generated AST =="
  rm -f "${BYTECODE_OBJECT}" "${EXECUTABLE}"
  (
    cd "${ROOT_DIR}"
    ocamlc -c -w -23-26-27-32-34-39 -o "${BYTECODE_OBJECT}" "${GENERATED_FILE}"
  )
  step_status=$?
  if [ "${step_status}" -ne 0 ]; then
    echo "Typecheck failed with status ${step_status}."
    exit "${step_status}"
  fi
  echo "Typecheck succeeded."
  echo

  echo "== Step 3: Build executable and run it =="
  (
    cd "${ROOT_DIR}"
    ocamlc -w -23-26-27-32-34-39 -o "${EXECUTABLE}" "${GENERATED_FILE}"
  )
  step_status=$?
  if [ "${step_status}" -ne 0 ]; then
    echo "Executable build failed with status ${step_status}."
    exit "${step_status}"
  fi
  echo "-- program output --"
  "${EXECUTABLE}"
  step_status=$?
  if [ "${step_status}" -ne 0 ]; then
    echo
    echo "Execution failed with status ${step_status}."
    exit "${step_status}"
  fi
  echo
  echo "Execution succeeded."
} 2>&1 | tee "${DUMP_FILE}"
status=${PIPESTATUS[0]}
set -e

if [ "${status}" -eq 0 ]; then
  echo
  echo "Pipeline succeeded."
else
  echo
  echo "Pipeline failed with status ${status}."
fi
echo "Full log available at: ${DUMP_FILE}"
exit "${status}"
