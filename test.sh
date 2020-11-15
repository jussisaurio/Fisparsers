#!/bin/bash
set -uo pipefail

# Using output of compiled executables as actual values, expected values are read from expected.txt
# Unfortunately return values get converted to 8-bit unsigned ints so e.g. -2 is 254.

expectedArr=()
for line in $(cat ./tests/expected.txt); do
    expectedArr+=($line)
done

files=()
for file in ./tests/*.c; do
    files+=($file)
done

dotnet run c "${files[@]}"
if [ $? -eq 1 ]
then
    rm ./tests/*.s
    exit 1
fi

i=0
for asmfile in ./tests/*.s; do
    exe="$asmfile.out"
    gcc $asmfile -o $exe
    $exe
    actual=$?
    echo "$file: expected ${expectedArr[i]}, got $actual"
    rm $asmfile
    rm $exe
    ((i=i+1))
done