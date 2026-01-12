#!/bin/bash

clear; make typer;

rm -f output.txt

output=$(./typer.exe test/unit_tests_debug.ml > output.txt 2> >(tee -a output.txt >&2))

res=$?

if [ ${res} -eq 0 ]; then
    echo 'debug successful'
    echo 'typed version available at: test/unit_tests_debug_typed.ml'
    echo 'compiled version available at: test/unit_tests_debug_compiled.ml'
else
    echo 'debug failed'
    # echo "$output"
    # cat output.txt
fi

# code output.txt
