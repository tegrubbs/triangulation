#!/bin/bash
files=("main" "genCircle" "genGrid")

for f in $files
do
    rm $f
done

make clean
make all
    
./genGrid
./main

