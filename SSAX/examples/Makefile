#	    Examples of SSAX parsing and SXML processing
#
# This Makefile assumes the use of GNU make, which is to be invoked
# as follows:
#    make PLATFORM=<platform> <target> ...
#
# where <platform> is one of the following:
#
#       biglooi		Bigloo interpreter
#       biglooc		Bigloo compiler
#       scmi		SCM interpreter
#       petite		Petite Chez interpreter
# 
# and <target> is explained in the README file in this directory.
# Additional targets:
# 	all			All pertinent targets
#	clean			Clean the working files and other garbage
#
# Not all of the targets are useful with all platforms.
# <target> may be omitted. In that case, all the targets
# relevant for the platform are assumed.
#
# $Id: Makefile,v 1.17 2004/08/06 23:05:40 oleg Exp $


# Directory where all the "included" modules are located
LIBDIR=../lib
TMPSCM=/tmp/a.scm	# Used for Bigloo and Petite

PLATFORM = scmi		# Default platform

# Bigloo-specific  part

BFLAGS=-s -I $(LIBDIR) -eval "(set! *load-path* (cons \"$(LIBDIR)\" *load-path*))" -call/cc -farithmetic -unsafea
BIGLOO=bigloo
#BFLAGS=-I $(LIBDIR) -Obench -farithmetic -unsafe
#BOPTFLAGS=-O3 -copt "-O3 -fomit-frame-pointer -mcpu=i686"

#------------------------------------------------------------------------
#	How to make the targets with different Scheme systems,
#		given the import list of "modules" (files)
#
# The 'functions' below take three parameters:
# the list of dependent files, the target (without .scm suffix),
# and the epilogue (usually empty).
# The rule must return a command to interpret or compile the target.
# The rules will be invoked as
#    $(call make-gambi,myenv.scm catch-error.scm,vmyenv,)
# For more details, see Chapter 'Functions' from 'make' info pages

make-gambi= gsi -e "(include \"$(LIBDIR)/myenv.scm\")" \
	    $(foreach file,$(1),-e "(include \"$(LIBDIR)/$(file)\")") \
	    $(2).scm $(3)

make-biglooi=$(BIGLOO) $(BFLAGS) -i \
	 -eval "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")))" \
	$(2).scm $(3)

make-biglooc=echo "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")) \
	         (include \"$(strip $(2)).scm\"))" > $(TMPSCM); \
	     $(BIGLOO) $(BFLAGS) -o $(2) $(TMPSCM) && rm -f $(TMPSCM) && \
	     ./$(strip $(2))

make-scmi= scm -b -r5 -l $(LIBDIR)/myenv-scm.scm \
	   $(foreach file,$(1),-l $(LIBDIR)/$(file)) \
	   -l $(2).scm $(3)

make-petite= echo "(exit 101)" | petite $(LIBDIR)/myenv-chez.scm \
	   $(foreach file,$(1), $(LIBDIR)/$(file)) \
	   $(2).scm $(3); test $$? -eq "101"

# can use define ... endef instead...

# Filter the dependency list and do other substitutions
# We filter out the files that overlap the prelude myenv*

filter-scmi= $(filter-out assert.scm define-opt.scm control.scm\
	       output.scm,$(1))

filter-petite=$(filter-scmi)
filter-biglooi=$(patsubst SSAX-code.scm,SSAX-expanded.scm,\
	          $(filter-out assert.scm define-opt.scm control.scm\
	             output.scm,$(1)))

#------------------------------------------------------------------------

biglooi-targets=sxml-db-conv \
	sxml-nesting-depth-label \
	sxml-to-sxml
#	parent-pointers \
	run-daml-parse-unparse \
	validate-doctype-simple \
	pull-punct-sxml \
	apply-templates \
	html-parser \
	sxslt-advanced  \

biglooc-targets=run-remove-markup-bigloo run-outline-bigloo run-sxml-bigloo

scmi-targets=remove-markup outline run-sxml \
	html-parser \
	daml-parse-unparse \
	sxslt-advanced  \
	apply-templates \
	sxml-db-conv \
	parent-pointers \
	validate-doctype-simple \
	sxml-nesting-depth-label \
	sxml-to-sxml

petite-targets=remove-markup outline run-sxml \
	html-parser \
	daml-parse-unparse \
	sxslt-advanced  \
	pull-punct-sxml \
	apply-templates \
	sxml-db-conv \
	parent-pointers \
	validate-doctype-simple \
	sxml-nesting-depth-label \
	sxml-to-sxml

.PHONY: all $($(PLATFORM)-targets)
all: $($(PLATFORM)-targets)

$($(PLATFORM)-targets):
	$(call make-$(PLATFORM),\
	       $(call filter-$(PLATFORM),$(strip $($@-depend))),\
	       $@,$($@-$(PLATFORM)-post))

clean:
	rm -f *_.c *.o core a.out /tmp/a.xml $(TMPSCM) \
	$($(PLATFORM)-targets) \
	$(addsuffix .c,$($(PLATFORM)-targets)) \
	$(addsuffix .o,$($(PLATFORM)-targets))

# The file target-dependencies is included in this Makefile at the end

target-dependencies: build-dependencies.scm packages.scm ../lib/packages.scm
	scm -r5 -l $(LIBDIR)/myenv-scm.scm -l build-dependencies.scm


#------------------------------------------------------------------------
#		Specific targets and their IMPORTS

# SCM-specific part

remove-markup-scmi-post=-l run-remove-markup.scm \
	-e '(main (list "myself" "xml/ddn.rdf"))'

outline-scmi-post=-l run-outline.scm \
	-e '(main (list "myself" "xml/total_weather.xsl"))'

run-sxml-scmi-post=-e '(main (list "myself" "xml/OMF-sample.xml"))'

# Petite-specific part

remove-markup-petite-post=run-remove-markup.scm \
	 `echo '(main (list "myself" "xml/ddn.rdf"))' > $(TMPSCM) && echo $(TMPSCM)`

outline-petite-post=run-outline.scm \
	`echo  '(main (list "myself" "xml/total_weather.xsl"))' > $(TMPSCM) && echo $(TMPSCM)`

run-sxml-petite-post=`echo '(main (list "myself" "xml/OMF-sample.xml"))' > $(TMPSCM) && echo $(TMPSCM)`

# Bigloo-specific part

run-remove-markup-bigloo: run-remove-markup-bigloo.scm \
		run-remove-markup.scm remove-markup.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@

run-outline-bigloo: run-outline-bigloo.scm run-outline.scm outline.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@

run-sxml-bigloo: run-sxml-bigloo.scm run-sxml.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@

# sxml-to-sxml: sxml-to-sxml.scm
# 	echo "(exit 0)" | petite $(LIBDIR)/myenv-chez.scm  \
# 	$(LIBDIR)/srfi-13-local.scm \
# 	$(LIBDIR)/util.scm \
# 	sxml-to-sxml.scm


# Not in the ./packages.scm

run-sxml-depend=srfi-13-local.scm char-encoding.scm util.scm look-for-str.scm input-parse.scm SSAX-code.scm

html-parser-depend=srfi-13-local.scm catch-error.scm char-encoding.scm util.scm look-for-str.scm input-parse.scm SSAX-code.scm

daml-parse-unparse-depend=assert.scm output.scm ppretty-prints.scm SXML-tree-trans.scm srfi-13-local.scm util.scm char-encoding.scm ssax-warn-vanilla.scm parser-errors-vanilla.scm define-opt.scm input-parse.scm look-for-str.scm control.scm SSAX-code.scm 

#sxml-to-sxml-depend= #srfi-13-local.scm #util.scm
#sxml-nesting-depth-label-depend= SXML-tree-trans.scm

include target-dependencies

