#!/bin/bash

clear; make typer;

rm -f output.txt

output=$((cd demo; dune clean; dune build; dune exec ./test_debug.exe) > output.txt 2> >(tee -a output.txt >&2))

res=$?

if [ ${res} -eq 0 ]; then
    echo 'test_debug successful'
else
    echo 'test_debug failed'
    # echo "$output"
    # cat output.txt
fi

# code output.txt
