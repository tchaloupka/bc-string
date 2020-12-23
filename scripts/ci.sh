#!/bin/bash

SRC_FILES="-Isource
    source/bc/core/system/linux/elf.d
    source/bc/core/system/linux/execinfo.d
    source/bc/core/system/linux/dwarf.d
    source/bc/core/system/backtrace.d
    source/bc/core/demangle.d
    source/bc/core/memory.d
    source/bc/core/intrinsics.d
    source/bc/core/traits.d
    source/bc/internal/utf.d
    source/bc/string/format.d
    source/bc/string/string.d
    source/bc/string/package.d
    source/bc/string/numeric.d"

set -v -e -o pipefail

if [ -z $DC ]; then DC="dmd"; fi

if [ $DC = "ldc2" ]; then DC="ldmd2"; fi

rm -f bc-string-*

if [ "$COVERAGE" = true ]; then
    $DC -version=CI_MAIN -cov -debug -g -unittest -w -vcolumns -of=bc-string-cov $SRC_FILES
    ./bc-string-cov
    wget https://codecov.io/bash -O codecov.sh
    bash codecov.sh
else
    # test with dub
    dub test --compiler=$DC

    # test release build
    echo "Building release test build"
    $DC -version=CI_MAIN -release -g -O -w -mcpu=native -inline -of=bc-string-release-test $SRC_FILES
    ./bc-string-release-test

    echo "Building betterC unittest runner"
    $DC -c -version=CI_MAIN -debug -unittest -g -w -vcolumns -betterC -of=bc-string.o $SRC_FILES
    $DC -ofbc-string-bc-test bc-string.o -g -betterC
    ./bc-string-bc-test

    echo "Building betterC test build"
    $DC -c -version=CI_MAIN -debug -g -w -vcolumns -betterC -of=bc-string.o $SRC_FILES
    $DC -ofbc-string-bc-test bc-string.o -g -betterC
    ./bc-string-bc-test

    echo "Building betterC release build"
    $DC -c -version=CI_MAIN -release -g -O -boundscheck=off -w -vcolumns -betterC -of=bc-string.o $SRC_FILES
    $DC -ofbc-string-bc-test bc-string.o -g -betterC
    ./bc-string-bc-test
fi
