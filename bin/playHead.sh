#!/usr/bin/env bash

# generate new bot at head
sbt stage

# update head
cp -rf target/universal/stage/ brutaltester/head

# run games locally
cd brutaltester
java -jar cg-brutaltester.jar -r "java -jar cg-ww.jar" -p1 "./head/bin/player" -p2 "./best/bin/player" -t 2 -n 100 -l "./logs/"
cd -