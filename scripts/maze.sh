#!/bin/bash

cd `dirname "$0"`
cd ..
./asm -i demo/maze.asm -o maze.bin -c || exit 1

stty -icanon -echo min 0 time 0
./eplayer maze.bin
stty sane

rm maze.bin
