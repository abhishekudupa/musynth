PROJECTROOT=$(realpath .)
SRCDIR=$(PROJECTROOT)/src

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep
OCAMLCFLAGS=-g -cc g++

INCDIRS+=-I $(SRCDIR)
BINDIR=$(PROJECTROOT)/bin
.PHONY:				clean opt all default byte

GENSRC= \
	musynthParser.ml \
	musynthParser.mli \
	musynthLexer.ml \
	musynthLexer.mli \


MODULES= \
	musynthTypes \
	musynthAST \
	musynthUtils \
	musynthParser \
	musynthLexer \
	musynthSymTab \
	musynthASTChecker \
	musynthFrontEnd \
	main \

EXES=musynth

ML=$(foreach f, $(MODULES), $(SRCDIR)/$(f).ml)
MLI=$(foreach f, $(MODULES), $(SRCDIR)/$(f).mli)
CMI=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmi)
CMO=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmo)
CMX=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmx)
DOTO=$(foreach f, $(MODULES), $(SRCDIR)/$(f).o)
ABSGENS=$(foreach f, $(GENSRC), $(SRCDIR)/$(f))
BYTEEXES=$(foreach f, $(EXES), $(BINDIR)/$(f).byte)
OPTEXES=$(foreach f, $(EXES), $(BINDIR)/$(f).opt)
DEPEND=$(SRCDIR)/depend


default: all

# targets
all : byte opt

byte : $(DEPEND) $(CMO) $(BYTEEXES) $(OPTEXES)

$(BINDIR)/%.byte : $(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CMO) -o $@

opt : $(DEPEND) $(CMX) $(OPTEXES)

$(BINDIR)/%.opt : $(CMX)
	$(OCAMLOPT) $(CMX) -o $@

$(DEPEND) : $(MLI) $(ML)
	$(OCAMLDEP) $(INCDIRS) $(MLI) $(ML) > $@

VPATH=$(PROJECTROOT)/src

$(SRCDIR)/musynthParser.ml $(SRCDIR)/musynthParser.mli : $(SRCDIR)/musynthParser.mly
	$(OCAMLYACC) $<

$(SRCDIR)/musynthLexer.ml $(SRCDIR)/musynthLexer.mli : $(SRCDIR)/musynthLexer.mll
	$(OCAMLLEX) $<; touch $(SRCDIR)/musynthLexer.mli

$(SRCDIR)/musynthLexer.cmi : $(SRCDIR)/musynthLexer.ml
	$(OCAMLC) -i $(INCDIRS) $< > $(SRCDIR)/musynthLexer.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $(SRCDIR)/musynthLexer.mli

# template rules
$(SRCDIR)/%.cmo : $(SRCDIR)/%.cmi

$(SRCDIR)%.cmi : $(SRCDIR)/%.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<

$(SRCDIR)/%.cmo : $(SRCDIR)/%.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<

$(SRCDIR)/%.cmx : $(SRCDIR)/%.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $(INCDIRS) $<


ifneq ($(MAKECMDGOALS), clean)
-include $(DEPEND)
endif

clean:
	rm -rf $(CMO) $(CMI) $(CMX) $(DOTO) $(ABSGENS) $(DEPEND) $(BYTEEXES) $(OPTEXES)
