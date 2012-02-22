#!/bin/sh

. $(dirname $0)/env.sh

export SCALA_VERSION=2.9.1
export ZALUUM_VERSION=0.8.1-SNAPSHOT

build $*
