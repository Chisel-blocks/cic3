#!/bin/sh
git submodule update --init --recursive
git submodule update --recursive

for i in dsptools hbwif; do
    cd $i && sbt publish-local && cd ..
done
exit 0

