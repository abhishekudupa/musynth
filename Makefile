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


default: all

-include Makefile.inc
