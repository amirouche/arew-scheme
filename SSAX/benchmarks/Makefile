# Makefile for running benchmarks
#
# You need to obtain Expat (e.g., from expat.sourceforge.net)
# and adjust the LIBS= variable below to correctly include the Expat
# library.
#
# Targets:
# bench1 -- comparing SSAX and Expat performance in "untagging"
#	    an XML document representing a full binary tree

# $Id: Makefile,v 1.5 2003/04/29 01:40:35 oleg Exp $

# Directory where all the "included" modules are located
LIBDIR=../lib

BPLATFORM_SPECIFIC=-copt "-fomit-frame-pointer -mcpu=i686"
BFLAGS=-I $(LIBDIR) -rm -Obench -farithmetic -unsafe $(BPLATFORM_SPECIFIC)
BIGLOO=bigloo

# C flags, location for the Expat library
CC = gcc
CPPFLAGS = -I /usr/local/include
LDFLAGS = 
LIBS = -L /usr/local/lib -lexpat
CFLAGS = -O2 -pipe


%: %.o rusage.o
	$(CC) -o $@ $@.o rusage.o $(LDFLAGS) $(LIBS)


# Main targets
bench1:	run-string-value run-string-value-by-one \
	run-string-value-ssax

# Prepare test files

bench1-file15.xml: make-file-bench1.scm
	$(BIGLOO) -i $(BFLAGS)  \
	make-file-bench1.scm "((depth . 15))" > $@

bench1-file16.xml: make-file-bench1.scm
	$(BIGLOO) -i $(BFLAGS)  \
	make-file-bench1.scm "((depth . 16))" > $@


# Subordinate targets
run-string-value: string-value.o string-value \
	bench1-file15.xml bench1-file16.xml
	./string-value  bench1-file15.xml
	./string-value  bench1-file16.xml

run-string-value-by-one: string-value-by-one.o string-value-by-one \
	bench1-file15.xml bench1-file16.xml
	./string-value-by-one  bench1-file15.xml
	./string-value-by-one  bench1-file16.xml

run-string-value-ssax: string-value-ssax \
	bench1-file15.xml bench1-file16.xml
	./string-value-ssax  bench1-file15.xml
	./string-value-ssax  bench1-file16.xml

string-value-ssax: string-value-ssax.scm rusage.o
	$(BIGLOO) $(BFLAGS) string-value-ssax.scm rusage.o -o $@


string-value-ssax-ss: string-value-ssax-ss.scm read-NCName-ss.scm \
	substring-symbol.c rusage.o 
	$(BIGLOO) $(BFLAGS) string-value-ssax-ss.scm rusage.o -o $@

clean:
	rm -f *.o core \
#	bench1-file15.xml \
#	bench1-file16.xml \
	string-value \
	string-value-by-one \
	string-value-ssax \
	string-value-ssax.c \
	string-value-ssax-ss \
	string-value-ssax-ss.c
