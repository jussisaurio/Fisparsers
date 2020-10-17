#!/bin/bash
set -uo pipefail

# Just echoing the return values without assertions for now
# Unfortunately return values get converted to 8-bit unsigned ints so e.g. -2 is 254.

for file in ./tests/*.c; do
    asmfile="$file-asm.s"
    exe="$file.out"
    dotnet run c $file $asmfile
    gcc $asmfile -o $exe
    $exe
    echo $?
    rm $asmfile
    rm $exe
done