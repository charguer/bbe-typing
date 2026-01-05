#!/bin/bash

clear; make typer;

rm -f output.txt

./typer.exe test/unit_tests_debug.ml > output.txt 2> >(tee -a output.txt >&2)

res=$?

if [ ${res} -eq 0 ]; then
    echo 'debug successful'
    echo 'test/unit_tests_debug_typed.ml'
else
    echo 'debug failed'
    cat output.txt
fi

# code output.txt
