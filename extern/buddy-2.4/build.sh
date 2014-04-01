#!/bin/bash

PWD=`/bin/pwd`

PREFIX=$PWD/build/

CFLAGS=-O3 CXXFLAGS=-O3 ./configure --prefix=$PREFIX --libdir=$PREFIX --bindir=$PREFIX
make -j 8
make install
make distclean

