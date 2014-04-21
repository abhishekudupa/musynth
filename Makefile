PROJECTROOT=$(realpath .)
SRCDIR=$(PROJECTROOT)/src
MAKEFLAGS+=-Oline

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
	musynthOptions \
	musynthDebug \
	musynthAST \
	musynthUtils \
	musynthParser \
	musynthLexer \
	musynthSymTab \
	musynthASTChecker \
	musynthChannel \
	musynthASTLower \
	musynthLtl \
	musynthSafety \
	musynthBDDManager \
	musynthBDDEncoder \
	musynthMC \
	musynthFrontEnd \
	main \

EXES=musynth

ML=$(foreach f, $(MODULES), $(SRCDIR)/$(f).ml)
BASEML=$(foreach f, $(ML), `basename $(f)`)
MLI=$(foreach f, $(MODULES), $(SRCDIR)/$(f).mli)
BASEMLI=$(foreach f, $(MLI), `basename $(f)`)
CMI=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmi)
BASECMI=$(foreach f, $(CMI), `basename $(f)`)
CMO=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmo)
BASECMO=$(foreach f, $(CMO), `basename $(f)`)
CMX=$(foreach f, $(MODULES), $(SRCDIR)/$(f).cmx)
BASECMX=$(foreach f, $(CMX), `basename $(f)`)
DOTO=$(foreach f, $(MODULES), $(SRCDIR)/$(f).o)
ABSGENS=$(foreach f, $(GENSRC), $(SRCDIR)/$(f))
BYTEEXES=$(foreach f, $(EXES), $(BINDIR)/$(f).byte)
OPTEXES=$(foreach f, $(EXES), $(BINDIR)/$(f).opt)
DEPEND=$(SRCDIR)/depend

VPATH=$(SRCDIR)

default: all

# targets
all : byte opt

byte : $(DEPEND) $(CMO) $(BYTEEXES) $(OPTEXES)

opt : $(DEPEND) $(CMX) $(OPTEXES)

$(BINDIR)/%.byte : $(CMO)
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlc] $(BASECMO)\n\t--> `basename $@`"; tput setaf 1;\
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -custom cudd.cma ltl3ba.cma $(CMO) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@; \
	tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -custom cudd.cma ltl3ba.cma $(CMO) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@
endif

$(BINDIR)/%.opt : $(CMX)
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlopt] $(BASECMX)\n\t--> `basename $@`"; tput setaf 1;\
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkall cudd.cmxa ltl3ba.cmxa $(CMX) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@; \
	tput sgr0
else
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkall cudd.cmxa ltl3ba.cmxa $(CMX) \
		-ccopt -lcuddcaml -ccopt -lltl3ba -o $@ 
endif

$(DEPEND) : $(MLI) $(ML)
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamldep] $(BASEMLI) $(BASEML)\n\t--> depend"; tput setaf 1;\
	$(OCAMLDEP) $(INCDIRS) $(MLI) $(ML) > $@; \
	tput sgr0
else
	$(OCAMLDEP) $(INCDIRS) $(MLI) $(ML) > $@
endif

$(SRCDIR)/musynthParser.ml $(SRCDIR)/musynthParser.mli : $(SRCDIR)/musynthParser.mly
	$(OCAMLYACC) -v $<

$(SRCDIR)/musynthLexer.ml $(SRCDIR)/musynthLexer.mli : $(SRCDIR)/musynthLexer.mll
	$(OCAMLLEX) $<; touch $(SRCDIR)/musynthLexer.mli

$(SRCDIR)/musynthLexer.cmi : $(DEPEND) $(SRCDIR)/musynthParser.cmi

$(SRCDIR)/musynthLexer.cmi : $(SRCDIR)/musynthLexer.ml
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	$(OCAMLC) -i $(INCDIRS) $< > $(SRCDIR)/musynthLexer.mli; \
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $(SRCDIR)/musynthLexer.mli; \
	tput sgr0
else
	$(OCAMLC) -i $(INCDIRS) $< > $(SRCDIR)/musynthLexer.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $(SRCDIR)/musynthLexer.mli
endif

$(SRCDIR)/%.cmo : $(DEPEND)
$(SRCDIR)/%.cmx : $(DEPEND)
$(SRCDIR)/%.cmi : $(DEPEND)

# template rules
$(SRCDIR)/%.cmo : $(DEPEND) $(SRCDIR)/%.cmi

$(SRCDIR)%.cmi : $(SRCDIR)/%.mli
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<; \
	tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<
endif

$(SRCDIR)/%.cmo : $(SRCDIR)/%.ml
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<; \
	tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<
endif

$(SRCDIR)/%.cmx : $(SRCDIR)/%.ml
ifeq "x$(VERBOSE_BUILD)" "x"
	@echo "[ocamlopt] `basename $<` --> `basename $@`"; tput setaf 1;\
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $(INCDIRS) $<; \
	tput sgr0
else
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $(INCDIRS) $<
endif


ifneq ($(MAKECMDGOALS), clean)
-include $(DEPEND)
endif

clean:
	@echo -n "Cleaning..."; \
	rm -rf $(CMO) $(CMI) $(CMX) $(DOTO) $(ABSGENS) $(DEPEND) $(BYTEEXES) $(OPTEXES) src/*.annot src/musynthParser.output; \
	echo " done!"
