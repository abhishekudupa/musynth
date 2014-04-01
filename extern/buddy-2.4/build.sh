#!/bin/bash

PREFIX=./build/

CFLAGS=-O3 CXXFLAGS=-O3 ./configure --prefix=$PREFIX --libdir=$PREFIX --bindir=$PREFIX
make -j 8
make install
