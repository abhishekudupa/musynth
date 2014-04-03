#!/bin/bash

PWD=`/bin/pwd`
NUMCORES=`cat /proc/cpuinfo | grep processor | wc -l`

PREFIX=$PWD/build/

CFLAGS="-O3 -fPIC" CXXFLAGS="-O3 -fPIC" ./configure --prefix=$PREFIX --libdir=$PREFIX --bindir=$PREFIX
make -j $NUMCORES
make install
make clean
make distclean

