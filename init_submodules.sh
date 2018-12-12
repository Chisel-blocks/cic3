#!/bin/sh
DIR="$( cd "$( dirname $0 )" && pwd )"
git submodule update --init

cd $DIR/rocket-chip
git submodule update --init chisel3
git submodule update --init firrtl
git submodule update --init hardfloat

##sbt publishig
cd $DIR/rocket-chip/firrtl
sbt publishLocal
sbt assembly

cd $DIR/rocket-chip/chisel3
sbt publishLocal

cd $DIR/rocket-chip/
sbt publishLocal

cd $DIR/hbwif
sbt publishLocal

cd $DIR/eagle_serdes
git submodule update --init --recursive serdes_top
sbt publishLocal

cd $DIR/dsptools
git submodule update --init --recursive
sbt publishLocal

cd $DIR/ofdm
git submodule update --init --recursive
sbt publishLocal

exit 0

