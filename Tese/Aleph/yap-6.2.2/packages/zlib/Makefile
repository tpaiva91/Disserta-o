################################################################
# Makefile template for SWI-Prolog CLIB package
#
# This template is used by configure to create Makefile.  See
# the file INSTALL for further installation instructions.
#
# License: LGPL
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
LIBS=-lz                                                            


SO=so
#4.1VPATH=.:./OPTYap
CWD=$(PWD)
#

LD=	gcc -shared -export-dynamic
LDFLAGS= -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) -L$(abs_top_builddir)/packages/PLStream -lplstream -rdynamic  -L$(abs_top_builddir) -Wl,-R$(LIBDIR) -Wl,-R,$(YAPLIBDIR)

BINTARGET=$(DESTDIR)$(YAPLIBDIR)
PLTARGET=$(DESTDIR)$(SHAREDIR)

FINAL_BINTARGET=$(YAPLIBDIR)
FINAL_PLTARGET=$(SHAREDIR)

LIBPL=		$(srcdir)/zlib.pl
TARGETS=	zlib4pl.so

%.o: $(srcdir)/%.c
		$(CC) -c $(CFLAGS) $< -o $@

else	# SWI

PL=@PL@
PLBASE=@PLBASE@
PLARCH=@PLARCH@
PLINCL=@PLINCL@
PKGDOC=$(PLBASE)/doc/packages
PCEHOME=../xpce

CC=gcc
COFLAGS=@COFLAGS@
CWFLAGS=@CWFLAGS@
CMFLAGS=@CMFLAGS@
CIFLAGS=
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H
LIBS=-lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl -lz                                                            

DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
DOC=zlib
TEX=$(DOC).tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
RUNTEX=../../man/runtex

LD=@LD@
LDFLAGS=@LDSOFLAGS@

LIBPL=		
TARGETS=	 rlimit.so

endif

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

ZOBJ=		zlib4pl.o

all:		$(TARGETS)

nolib::
		@echo "WARNING: Could not find zlib library; skipped"

zlib4pl.so:	$(ZOBJ)
		$(LD) $(LDFLAGS) -o $@ $(ZOBJ) $(LIBS)

install:	$(TARGETS) $(LIBPL)
		mkdir -p $(BINTARGET)
		for f in $(TARGETS); do \
		  $(INSTALL_PROGRAM) $$f $(BINTARGET); \
		done
		mkdir -p $(PLTARGET)
		for f in $(LIBPL); do \
		  $(INSTALL_DATA) $$f $(PLTARGET); \
		done
		$(PL) -f none -g make -t halt

ln-install::
		$(MAKE) INSTALL_DATA="../ln-install" INSTALL_PROGRAM="../ln-install" install

rpm-install:	install

html-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 $(DOC).html $(DESTDIR)$(PKGDOC)

pdf-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 $(DOC).pdf $(DESTDIR)$(PKGDOC)

uninstall::
		(cd $(PLBASE)/lib/$(PLARCH) && rm -f $(TARGETS))
		(cd $(PLBASE)/library && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

################################################################
# Testing
################################################################

check::
		$(PL) -q -f $(srcdir)/test_zlib.pl -g test_zlib,halt -t 'halt(1)'

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
		mv html/index.html $(DOC).html
		rm -r html

$(TEX):		$(DOCTOTEX)

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
		$(PLTOTEX) $*.pl > $*.tex

################################################################
# Clean
################################################################

clean:
		rm -f $(ZOBJ) *~ *.o *% a.out core config.log

distclean:	clean
		rm -f $(TARGETS) config.cache config.h config.status Makefile
		rm -f $(DOC).aux $(DOC).log $(DOC).out $(DOC).toc
		rm -rf html
		rm -rf autom4te.cache
