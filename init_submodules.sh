#!/usr/bin/env bash
#Selective initialization of submodules
#Written by by Marko Kosunen, marko.kosunen@aalto.fi, 2017
DIR=$( cd `dirname $0` && pwd )
SUBMODULES="\
"

git submodule update --init
for module in $SUBMODULES; do
    cd ${DIR}/${module}
    if [ -f "./init_submodules.sh" ]; then
        ./init_submodules.sh
    fi
    sbt publishLocal
    cd ${DIR}
done
exit 0
