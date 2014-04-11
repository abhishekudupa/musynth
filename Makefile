PROJECTROOT=$(realpath .)
SRCDIR=$(PROJECTROOT)/src

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex.opt
OCAMLDEP=ocamldep.opt
OCAMLCFLAGS=-g -annot -cc g++ -ccopt -Wno-write-strings 
OCAMLOPTFLAGS=-cc g++ -ccopt -Wno-write-strings 

INCDIRS=-I $(SRCDIR) -I $(PROJECTROOT)/extern/mlcudd/release-2.2.0 
INCDIRS+=-I $(PROJECTROOT)/extern/ltl3ba-1.0.2
OCAMLCFLAGS+=$(INCDIRS)
OCAMLOPTFLAGS+=$(INCDIRS)

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
	musynthChannel \
	musynthASTLower \
	musynthFrontEnd \
	musynthBDD \
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
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -custom cudd.cma ltl3ba.cma $(CMO) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@

opt : $(DEPEND) $(CMX) $(OPTEXES)

$(BINDIR)/%.opt : $(CMX)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkall cudd.cmxa ltl3ba.cmxa $(CMX) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@ 

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
	rm -rf $(CMO) $(CMI) $(CMX) $(DOTO) $(ABSGENS) $(DEPEND) $(BYTEEXES) $(OPTEXES) src/*.annot
