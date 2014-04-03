# LTL3BA - Version 1.0.2 - December 2012

# Written by Denis Oddoux, LIAFA, France                                 
# Copyright (c) 2001  Denis Oddoux                                       
# Modified by Paul Gastin, LSV, France                                   
# Copyright (c) 2007  Paul Gastin                                        
# Modified by Tomas Babiak, FI MU, Brno, Czech Republic                  
# Copyright (c) 2012  Tomas Babiak                                       
#                                                                        
# This program is free software; you can redistribute it and/or modify   
# it under the terms of the GNU General Public License as published by   
# the Free Software Foundation; either version 2 of the License, or      
# (at your option) any later version.                                    
#                                                                        
# This program is distributed in the hope that it will be useful,        
# but WITHOUT ANY WARRANTY; without even the implied warranty of         
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          
# GNU General Public License for more details.                           
#                                                                        
# You should have received a copy of the GNU General Public License      
# along with this program; if not, write to the Free Software            
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
#                                                                        
# Based on the translation algorithm by Gastin and Oddoux,               
# presented at the 13th International Conference on Computer Aided       
# Verification, CAV 2001, Paris, France.                                 
# Proceedings - LNCS 2102, pp. 53-65                                     
#                                                                        
# Modifications based on paper by                                        
# T. Babiak, M. Kretinsky, V. Rehak, and J. Strejcek,                    
# LTL to Buchi Automata Translation: Fast and More Deterministic         
# presented at the 18th International Conference on Tools and            
# Algorithms for the Construction and Analysis of Systems (TACAS 2012)   

# modifications to build system for lib generation as well as 
# ocaml bindings by
# Abhishek Udupa, University of Pennsylvania, Copyright 2014
# audupa@cis.upenn.edu

# Set PATH to "bdd.h" BuDDy file.
BUDDY_INCLUDE=../buddy-2.4/build/include
# Set PATH to "libbdd.a" BuDDy file.
BUDDY_LIB=../buddy-2.4/build

# to obtain BuDDy run:
# $ cd some_directory
# $ wget https://downloads.sourceforge.net/project/buddy/buddy/BuDDy%202.4/buddy-2.4.tar.gz
# $ tar xzf buddy-2.4.tar.gz
# $ cd buddy-2.4
# $ ./configure
# $ make
# then set  
# BUDDY_INCLUDE=some_directory/buddy-2.4/src/
# BUDDY_LIB=some_directory/buddy-2.4/src/.libs/
# or run 
# $ make install 
# and use the default values of BUDDY_INCLUDE and BUDDY_LIB.

CC=g++
CXX=g++
CPPFLAGS=-fPIC -O3 -std=c++11 -DNXT -I$(BUDDY_INCLUDE)

LTL3BA=	parse.o lex.o main.o trans.o buchi.o cset.o set.o \
	mem.o rewrt.o cache.o alternating.o generalized.o optim.o queue.o \


LIBLTL3BAOBJS=	parse.o lex.o trans.o buchi.o cset.o set.o \
	mem.o rewrt.o cache.o alternating.o generalized.o optim.o queue.o \
	libmain.o \


default: all

all: ltl3ba libltl3ba

ltl3ba:	$(LTL3BA)
	$(CXX)  $(CPPFLAGS)  -o ltl3ba $(LTL3BA) -L$(BUDDY_LIB) -Wl,-Bstatic -lbdd -Wl,-Bdynamic

libltl3ba: libltl3ba.so libltl3ba.a

libltl3ba.so: $(LIBLTL3BAOBJS)
	rm -rf .temp2; mkdir .temp2; cp $(BUDDY_LIB)/libbdd.a .temp2; cd .temp2; ar -x libbdd.a
	$(CXX)  $(CPPFLAGS) -shared -o $@ $(LIBLTL3BAOBJS) .temp2/*.o
	rm -rf .temp2

libltl3ba.a: $(LIBLTL3BAOBJS)
	rm -rf .temp; mkdir .temp; cp $(BUDDY_LIB)/libbdd.a .temp; cd .temp; ar -x libbdd.a; \
		cd ..; ar -rcs $@ $(LIBLTL3BAOBJS) .temp/*.o; rm -rf .temp

$(LTL3BA): ltl3ba.h

libmain.o: main.cpp
	$(CXX)  $(CPPFLAGS)  -c -DLTL3BA_LIB_BUILD -o libmain.o main.cpp

.PHONY: clean all default

clean:
	rm -f ltl3ba *.o core libltl3ba.*
