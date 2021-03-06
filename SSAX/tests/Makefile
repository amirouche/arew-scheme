#		Testing my library, including SSAX
#
# This Makefile assumes the use of GNU make, which is to be invoked
# as follows:
#    make PLATFORM=<platform> <target> ...
#
# where <platform> is one of the following:
#
#       gambi		Gambit-C interpreter
#       gambc		Gambit-C compiler
#       biglooi		Bigloo interpreter
#       biglooc		Bigloo compiler
#       scmi		SCM interpreter
#       petite		Petite Chez interpreter
# 
# and <target> is one of the following:
#       vmyenv
#	vsrfi-13
#	vinput-parse
#	vsrfi-12
#	vland
#       vSXML-tree-trans
#	vSXML-to-HTML
#	vSSAX
#	vSSAX-exp		Use the macro-expanded code SSAX-expanded.scm 
# 	all			All of the above
#	clean			Clean the working files and other garbage
#
# Not all of the targets are useful with all platforms.
# <target> may be omitted. In that case, all the targets
# relevant for the platform are assumed.
#
# $Id: Makefile,v 1.9 2004/07/08 21:26:18 oleg Exp $

# Scheme library directory
LIBDIR=../lib
TMPSCM=/tmp/a.scm	# Used for Bigloo

PLATFORM = biglooi	# Default platform


#GAMBIT_LIBRARY=$(MYLIBRARY)

# Bigloo-specific  part

BFLAGS=-I $(LIBDIR) -eval "(set! *load-path* (cons \"$(LIBDIR)\" *load-path*))" -call/cc -farithmetic -unsafea
BIGLOO=bigloo
#BFLAGS=-I $(LIBDIR) -Obench -farithmetic -unsafe
#BOPTFLAGS=-O3 -copt "-O3 -fomit-frame-pointer -mcpu=i686"

#------------------------------------------------------------------------
#	How to make the targets with different Scheme systems,
#		given the import list of "modules" (files)
#
# The 'functions' below take two parameters:
# the list of dependent files and the target (without .scm suffix)
# The rule must return a command to interpret or compile the target.
# The rules will be invoked as
#    $(call make-gambi,myenv.scm catch-error.scm,vmyenv)
# For more details, see Chapter 'Functions' from 'make' info pages

make-gambi= gsi -e "(include \"$(LIBDIR)/myenv.scm\")" \
	    $(foreach file,$(1),-e "(include \"$(LIBDIR)/$(file)\")") \
	    $(2).scm

make-biglooi=$(BIGLOO) $(BFLAGS) -i \
	 -eval "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")))" \
	$(2).scm

make-biglooc=echo "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")) \
	         (include \"$(strip $(2)).scm\"))" > $(TMPSCM); \
	     $(BIGLOO) $(BFLAGS) -o $(2) $(TMPSCM) && rm -f $(TMPSCM) && \
	     ./$(strip $(2))

make-scmi= scm -b -r5 -l $(LIBDIR)/myenv-scm.scm \
	   $(foreach file,$(1),-l $(LIBDIR)/$(file)) \
	   -l $(2).scm

make-petite= echo "(exit 101)" | petite $(LIBDIR)/myenv-chez.scm \
	   $(foreach file,$(1), $(LIBDIR)/$(file)) \
	   $(2).scm; test $$? -eq "101"

#------------------------------------------------------------------------

gambi-targets=vmyenv vsrfi-13 vinput-parse vsrfi-12  \
	      vland \
	      vSXML-tree-trans vSXML-to-HTML vSSAX-exp 
biglooi-targets=vmyenv vsrfi-13 vinput-parse vsrfi-12  \
	      vland \
	      vSXML-tree-trans vSXML-to-HTML vSSAX-exp  #vext-io vipc

biglooc-targets=vmyenv vsrfi-13 vinput-parse vsrfi-12  \
	      vSXML-tree-trans vSXML-to-HTML vSSAX-exp  #vext-io vipc

scmi-targets=vmyenv vsrfi-13 vinput-parse  \
	      vSXML-tree-trans vSXML-to-HTML vSSAX  #vext-io vipc

petite-targets=vmyenv vsrfi-13 vinput-parse  \
	      vSXML-tree-trans vSXML-to-HTML vSSAX  #vext-io vipc

.PHONY: all $($(PLATFORM)-targets)
all: $($(PLATFORM)-targets)

clean:
	rm -f *_.c *.o core a.out $(TMPSCM) \
	$($(PLATFORM)-targets) \
	$(addsuffix .c,$($(PLATFORM)-targets)) \
	$(addsuffix .o,$($(PLATFORM)-targets))


#------------------------------------------------------------------------
#		Specific targets and their IMPORTS

vmyenv:
	$(call make-$(PLATFORM),\
	       catch-error.scm env.scm srfi-13-local.scm util.scm,\
	$@)


vsrfi-13:
	$(call make-$(PLATFORM),\
	       catch-error.scm srfi-13-local.scm,\
	$@)


vinput-parse: 
	$(call make-$(PLATFORM),\
	       catch-error.scm srfi-13-local.scm util.scm look-for-str.scm  \
	       char-encoding.scm input-parse.scm,\
	       $@)
vsrfi-12:
	$(call make-$(PLATFORM),\
	       srfi-12.scm,\
	       $@)

vreaddir:
	$(call make-$(PLATFORM),\
	       catch-error.scm,\
	       $@)

vtreap:
	$(call make-$(PLATFORM),\
	       catch-error.scm treap.scm,\
	       $@)

vmime:
	$(call make-$(PLATFORM),\
	       catch-error.scm srfi-13-local.scm util.scm input-parse.scm \
	       mime.scm,\
	       $@)

vhttp:
	$(call make-$(PLATFORM),\
	srfi-12.scm srfi-13-local.scm util.scm input-parse.scm \
	       http.scm mime.scm,\
	       $@)

vland:
	$(call make-$(PLATFORM),\
	       srfi-12.scm,\
	       $@)

vext-io:
	$(call make-$(PLATFORM),\
	       catch-error.scm,\
	       ,\
	       $@)

vipc:
	$(call make-$(PLATFORM),\
	       catch-error.scm,\
	       $@)

vSXML-tree-trans:
	$(call make-$(PLATFORM),\
	       srfi-13-local.scm util.scm SXML-tree-trans.scm,\
	       $@)

vSXML-to-HTML:
	$(call make-$(PLATFORM),\
	       srfi-13-local.scm util.scm SXML-tree-trans.scm \
	       SXML-to-HTML.scm,\
	       $@)
vSSAX:
	$(call make-$(PLATFORM),\
	       catch-error.scm srfi-13-local.scm util.scm \
	       look-for-str.scm char-encoding.scm input-parse.scm,\
	       $(LIBDIR)/SSAX)

vSSAX-exp:
	$(call make-$(PLATFORM),\
	       catch-error.scm srfi-13-local.scm util.scm \
	       look-for-str.scm char-encoding.scm input-parse.scm,\
	       $(LIBDIR)/SSAX-expanded)

#------------------------------------------------------------------------

# Library

# %:	%.o $(LIBRARY)
# 	gsc -f -l $(MYLINKFILE) -verbose $@.c
# 	$(CC) $(LDFLAGS) $(MYLINKFILE).o $@_.c -u ___io_open_file $^ \
# 	-R/usr/lib:/lib:/usr/local/lib -o $@ -lgambc $(EXT_LIBS) -lm

#gsi:	gsi.o $(LIBRARY)
#	gsc -f -l $(MYLINKFILE) -verbose gsi.c
#	$(CC) $(LDFLAGS) $(MYLINKFILE).o gsi_.c $^ \
#	-o $@ -lgambc $(EXT_LIBS) -lm


# check-compiled:	vreaddir vext-io
# #	./vendian_io
# 	./vreaddir
# 	./vext-io

# check-mit:
# 	scheme -load myenv-mit.scm util.scm env.scm vmyenv.scm
# 	scheme -load myenv-mit.scm util.scm input-parse.scm look-for-str.scm vinput-parse.scm
#	scheme -load myenv-mit.scm treap.scm vtreap.scm
#	scheme -load myenv-mit.scm vland.scm



