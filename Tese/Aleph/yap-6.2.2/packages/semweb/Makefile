################################################################
# Makefile template for SWI-Prolog RDF-DB package
#
# This template is used by configure to create Makefile.  See
# the file INSTALL for further installation instructions.
#
# License: LGPL
#
# Special targets:
#
#	ln-install		Install with symbolic links
#
# Author: Jan Wielemaker (wielemak@science.uva.nl)
################################################################

.SUFFIXES: .tex .dvi .doc .pl

ifeq (yap,yap)

prefix = /usr/local
exec_prefix = ${prefix}
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
abs_top_builddir = /home/tiago/Tese/Tese/Aleph/yap-6.2.2

srcdir=.

BINDIR = $(EROOTDIR)/bin
LIBDIR=${exec_prefix}/lib
YAPLIBDIR=${exec_prefix}/lib/Yap
SHAREDIR=$(ROOTDIR)/share/Yap

SHELL=/bin/bash
PL=YAPSHAREDIR=$(DESTDIR)$(SHAREDIR) YAPLIBDIR=$(DESTDIR)$(YAPLIBDIR) $(DESTDIR)$(BINDIR)/yap $(DESTDIR)$(YAPLIBDIR)/startup.yss
EXDIR=$(YAPLIBDIR)/examples/http
LN_S=ln -s
#
#
DEFS=-DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1
CC=gcc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I../.. -I$(srcdir)/../../include 
MKINDEX=true


SO=so
#4.1VPATH=.:./OPTYap
CWD=$(PWD)
#

LD=	gcc -shared -export-dynamic
LDFLAGS= -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) -L$(abs_top_builddir)/packages/PLStream -lplstream -rdynamic  -L$(abs_top_builddir) -Wl,-R$(LIBDIR) -Wl,-R,$(YAPLIBDIR)

BINTARGET=$(DESTDIR)$(YAPLIBDIR)
PLTARGET=$(DESTDIR)$(SHAREDIR)/semweb

FINAL_BINTARGET=$(YAPLIBDIR)
FINAL_PLTARGET=$(SHAREDIR)

%.o: $(srcdir)/%.c
		$(CC) -c $(CFLAGS) $< -o $@

else

PL=@PL@
PLBASE=@PLBASE@
PLARCH=@PLARCH@
PLINCL=@PLINCL@
PKGDOC=$(PLBASE)/doc/packages
PCEHOME=../xpce
LIBDIR=$(PLBASE)/library/semweb

CC=gcc
COFLAGS=@COFLAGS@
CWFLAGS=@CWFLAGS@
CMFLAGS=@CMFLAGS@
CIFLAGS=-I.
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H
LIBS=-lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl
NETLIBS=@NETLIBS@

PUBLICPL=swipl
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PUBLICPL) -q -s pltotex.pl -g pltotex --
DOC=semweb
TEX=$(DOC).tex	rdfturtle.tex rdfturtlewrite.tex rdfcompare.tex \
		rdfportray.tex rdflib.tex sparqlclient.tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
RUNTEX=../../man/runtex

LD=@LD@
LDFLAGS=@LDSOFLAGS@

endif

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

include $(srcdir)/common.mk

TARGETS=	rdf_db.so turtle.so

RDFDBOBJ=	rdf_db.o atom.o lock.o md5.o avl.o atom_map.o debug.o \
		hash.o murmur.o

all:		$(TARGETS)

rdf_db.so:	$(RDFDBOBJ)
		$(LD) $(LDFLAGS) -o $@ $(RDFDBOBJ) $(LIBS)
turtle.so:	turtle.o
		$(LD) $(LDFLAGS) -o $@ turtle.o $(LIBS)

turtle.o:	$(srcdir)/turtle.c $(srcdir)/turtle_chars.c

install:	$(TARGETS) $(LIBPL)
		mkdir -p $(BINTARGET)
		for f in $(TARGETS); do \
		  $(INSTALL) -m 755 $$f $(BINTARGET); \
		done
		mkdir -p $(PLTARGET)
		for f in $(DATA) $(LIBPL) $(srcdir)/README; do \
		  $(INSTALL_DATA) $$f $(PLTARGET); \
		done
		$(PL) -f none -g make -t halt

ln-install::
		$(MAKE) INSTALL=../ln-install \
			INSTALL_DATA=../ln-install \
			install

rpm-install:	install

html-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 $(DOC).html $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 $(srcdir)/modules.gif $(DESTDIR)$(PKGDOC)

pdf-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 $(DOC).pdf $(DESTDIR)$(PKGDOC)

uninstall::
		(cd $(PLBASE)/lib/$(PLARCH) && rm -f $(TARGETS))
		(cd $(PLBASE)/library && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

rdf_db.o:	$(srcdir)/quote.c $(srcdir)/unicode_map.c

################################################################
# Documentation
################################################################

pdf:		$(PDF)

$(DVI):		$(TEX)
		$(RUNTEX) $(DOC)

$(PDF):		$(TEX)
		$(RUNTEX) --pdf $(DOC)

html:		$(TEX)
		latex2html $(DOC)

$(TEX):		$(DOCTOTEX)

rdfturtle.tex:	$(srcdir)/rdf_turtle.pl
		$(PLTOTEX) --section 'library(semweb/rdf_turtle)'
rdfturtlewrite.tex:	$(srcdir)/rdf_turtle_write.pl
		$(PLTOTEX) --section 'library(semweb/rdf_turtle_write)'
rdfcompare.tex:	$(srcdir)/rdf_compare.pl
		$(PLTOTEX) --section 'library(semweb/rdf_compare)'
rdfportray.tex:	$(srcdir)/rdf_portray.pl
		$(PLTOTEX) --section 'library(semweb/rdf_portray)'
sparqlclient.tex:	$(srcdir)/sparql_client.pl
		$(PLTOTEX) --section 'library(semweb/sparql_client)'

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex

################################################################
# Check
################################################################

check::
		$(PL) -q -f $(srcdir)/test_rdf_db.pl -g test_rdf_db,halt -t 'halt(1)'
		$(PL) -q -f $(srcdir)/litmap_test.pl -g test,halt -t 'halt(1)'
		$(PL) -q -f $(srcdir)/test.pl -g run_tests,halt -t 'halt(1)'
		$(PL) -q -f $(srcdir)/test_turtle.pl -g test_turtle,halt -t 'halt(1)'


################################################################
# Clean
################################################################

clean:		texclean
		rm -f $(OBJ) *~ *.o *% a.out core config.log

texclean:
		rm -f $(TEX) $(DOC).aux $(DOC).log $(DOC).out $(DOC).toc \
		      $(DOC).ilg

distclean:	clean
		rm -f $(TARGETS) config.cache config.h config.status Makefile

