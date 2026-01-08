#!/bin/bash

# clear; make typer; exit;
clear; make typer;


rm -f output.txt

output=$(./typer.exe test/unit_tests_bbe.ml > output.txt 2> >(tee -a output.txt >&2))

res=$?

if [ ${res} -eq 0 ]; then
    echo 'successfully typechecked test/unit_tests_bbe.ml'
    echo 'test/unit_tests_bbe_typed.ml'
else
    echo 'debug failed'
    # cat 'output.txt'
fi


# code output.txt