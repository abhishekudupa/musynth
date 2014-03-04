OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep

OBJDIR=$(SRCDIR)
INCDIRS+=-I $(OBJDIR)
.PHONY:				clean opt all default byte

ML=$(foreach f, $(MODULES), $(SRCDIR)/$(f).ml)
MLI=$(foreach f, $(MODULES), $(SRCDIR)/$(f).mli)
CMI=$(foreach f, $(MODULES), $(OBJDIR)/$(f).cmi)
CMO=$(foreach f, $(MODULES), $(OBJDIR)/$(f).cmo)
CMX=$(foreach f, $(MODULES), $(OBJDIR)/$(f).cmx)
DOTO=$(foreach f, $(MODULES), $(OBJDIR)/$(f).o)
ABSGENS=$(foreach f, $(GENSRC), $(SRCDIR)/$(f))
DEPEND=$(OBJDIR)/depend

# template rules
$(SRCDIR)/%.ml : $(SRCDIR)/%.mll
	$(OCAMLLEX) $<

$(SRCDIR)/%.ml $(SRCDIR)/%.mli : $(SRCDIR)/%.mly
	$(OCAMLYACC) $<

$(SRCDIR)/%.mli : $(SRCDIR)/%.ml
	$(OCAMLC) -i $(INCDIRS) $< > $@

$(OBJDIR)/%.cmi : $(SRCDIR)/%.mli
	$(OCAMLC) -c $(INCDIRS) $<

$(OBJDIR)/%.cmo : $(SRCDIR)/%.mli $(OBJDIR)/%.cmi

$(OBJDIR)/%.cmo : $(SRCDIR)/%.ml
	$(OCAMLC) -c $(INCDIRS) $<

$(OBJDIR)/%.cmx : $(OBJDIR)/%.cmi
$(OBJDIR)/%.cmx : $(SRCDIR)/%.ml
	$(OCAMLOPT) -c $(INCDIRS) $<

# targets
all : byte opt

byte : $(DEPEND) $(CMO)

opt : $(DEPEND) $(CMX)

$(DEPEND) : $(ML) $(ABSGENS)
	$(OCAMLDEP) $(ML) $(ABSGENS) > $@

ifneq ($(MAKECMDGOALS), clean)
-include $(DEPEND)
endif

clean:
	rm -rf $(CMO) $(CMI) $(CMX) $(DOTO) $(ABSGENS) $(DEPEND)
