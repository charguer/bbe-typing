#!/bin/bash

# clear; make typer; exit;
clear; make typer;


rm -f output.txt

./typer.exe test/unit_tests_bbe.ml > output.txt 2> >(tee -a output.txt >&2)

res=$?

if [ ${res} -eq 0 ]; then
    echo 'successfully typechecked test/unit_tests_bbe.ml'
    echo 'test/unit_tests_bbe_typed.ml'
fi

# code output.txt