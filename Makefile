PROJECTROOT=$(realpath .)
SRCDIR=$(PROJECTROOT)/src
MAKEFLAGS+=-Oline

ECHO=/bin/echo

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex.opt
OCAMLDEP=ocamldep.opt
OCAMLCFLAGS=-g -annot -cc g++
OCAMLOPTFLAGS=-cc g++

INCDIRS=-I $(SRCDIR) -I $(PROJECTROOT)/extern/mlcudd/release-2.2.0 
OCAMLCFLAGS+=$(INCDIRS)
OCAMLOPTFLAGS+=$(INCDIRS) -inline 128

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
	musynthTrace \
	musynthParser \
	musynthLexer \
	musynthSymTab \
	musynthASTChecker \
	musynthChannel \
	musynthASTLower \
	musynthLtl \
	musynthBDDManager \
	musynthBDDEncoder \
	musynthMCUtils \
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
	@$(ECHO) -e "[ocamlc] $(BASECMO)\n\t--> `basename $@`"; tput setaf 1;\
	($(OCAMLC) $(OCAMLCFLAGS) -linkall -custom cudd.cma $(CMO) \
		-ccopt -lcuddcaml -o $@ || (tput sgr0 && exit 1)) && tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -linkall -custom cudd.cma $(CMO) \
		-ccopt -lcuddcaml -o $@
endif

$(BINDIR)/%.opt : $(CMX)
ifeq "x$(VERBOSE_BUILD)" "x"
	@$(ECHO) -e "[ocamlopt] $(BASECMX)\n\t--> `basename $@`"; tput setaf 1;\
	($(OCAMLOPT) $(OCAMLOPTFLAGS) -linkall cudd.cmxa $(CMX) \
		-ccopt -lcuddcaml -o $@ || (tput sgr0 && exit 1)) && tput sgr0
else
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -linkall cudd.cmxa $(CMX) \
		-ccopt -lcuddcaml -o $@ 
endif

$(DEPEND) : $(MLI) $(ML)
ifeq "x$(VERBOSE_BUILD)" "x"
	@$(ECHO) -e "[ocamldep] $(BASEMLI) $(BASEML)\n\t--> depend"; tput setaf 1;\
	($(OCAMLDEP) $(INCDIRS) $(MLI) $(ML) > $@ || (tput sgr0 && exit 1)) && tput sgr0
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
	@$(ECHO) -e "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	(($(OCAMLC) -i $(INCDIRS) $< > $(SRCDIR)/musynthLexer.mli && \
	 $(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $(SRCDIR)/musynthLexer.mli) || \
	(tput sgr0 && exit 1)) && tput sgr0
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
	@$(ECHO) -e "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	($(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $< || (tput sgr0 && exit 1)) && tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<
endif

$(SRCDIR)/%.cmo : $(SRCDIR)/%.ml
ifeq "x$(VERBOSE_BUILD)" "x"
	@$(ECHO) -e "[ocamlc] `basename $<` --> `basename $@`"; tput setaf 1;\
	($(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $< || (tput sgr0 && exit 1)) && tput sgr0
else
	$(OCAMLC) $(OCAMLCFLAGS) -c $(INCDIRS) $<
endif

$(SRCDIR)/%.cmx : $(SRCDIR)/%.ml
ifeq "x$(VERBOSE_BUILD)" "x"
	@$(ECHO) -e "[ocamlopt] `basename $<` --> `basename $@`"; tput setaf 1;\
	($(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(INCDIRS) $< || (tput sgr0 && exit 1)) && tput sgr0 \
	tput sgr0
else
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(INCDIRS) $<
endif


ifneq ($(MAKECMDGOALS), clean)
-include $(DEPEND)
endif

clean:
	@$(ECHO) -n "Cleaning..."; \
	rm -rf $(CMO) $(CMI) $(CMX) $(DOTO) $(ABSGENS) $(DEPEND) $(BYTEEXES) $(OPTEXES) src/*.annot src/musynthParser.output; \
	$(ECHO) " done!"
