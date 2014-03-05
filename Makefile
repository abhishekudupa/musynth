PROJECTROOT=$(realpath .)
SRCDIR=$(PROJECTROOT)/src

GENSRC= \
	musynthParser.ml \
	musynthParser.mli \
	musynthLexer.ml \


MODULES= \
	musynthAST \
	musynthParser \
	musynthLexer \
	musynthFrontEnd \
	main \

EXE=musynth

default: all

-include Makefile.inc
