#!/bin/bash

if [ "$1" == "build" ]; then
    echo "Building problem"$2
    ghc problem$2
elif [ "$1" == "clean" ]; then
    echo "Cleaning problem"$2
    rm problem$2 problem$2.hi problem$2.o
elif [ "$1" == "run" ]; then
    ./build.sh clean $2
    ./build.sh build $2
    echo "Running problem"$2
    echo "------------------"
    ./problem$2
fi
