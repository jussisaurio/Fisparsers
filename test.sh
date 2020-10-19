#!/bin/bash
set -uo pipefail

# Using output of compiled executables as actual values, expected values are read from expected.txt
# Unfortunately return values get converted to 8-bit unsigned ints so e.g. -2 is 254.

expectedArr=()
for line in $(cat ./tests/expected.txt); do
    expectedArr+=($line)
done

i=0
for file in ./tests/*.c; do
    asmfile="$file-asm.s"
    exe="$file.out"
    dotnet run c $file $asmfile
    gcc $asmfile -o $exe
    $exe
    actual=$?
    echo "$file: expected ${expectedArr[i]}, got $actual"
    rm $asmfile
    rm $exe
    ((i=i+1))
done