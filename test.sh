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

passed=0
failed=0
for asmfile in ./tests/*.s; do
    exe="$asmfile.out"
    gcc $asmfile -o $exe
    $exe
    actual=$?
    if [ $actual -ne ${expectedArr[i]} ]
    then
        echo "${files[i]}: expected ${expectedArr[i]}, got $actual"
        ((failed=failed+1))
    else
        ((passed=passed+1))
    fi
    rm $asmfile
    rm $exe
    ((i=i+1))
done

echo "$passed tests passed, $failed failed"
if [ $failed -gt 0 ]
then
    exit 1
else
    exit 0
fi