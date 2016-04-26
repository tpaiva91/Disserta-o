################################################################
# SWI-Prolog `HTTP' package
# Author:    Jan Wielemaker. J.Wielemaker@cs.vu.nl
# Copyright: GPL (see COPYING or www.gnu.org
################################################################

.SUFFIXES: .txt .tex .dvi .doc .pl

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
PLTARGET=$(DESTDIR)$(SHAREDIR)/http

FINAL_BINTARGET=$(YAPLIBDIR)
FINAL_PLTARGET=$(SHAREDIR)

%.o: $(srcdir)/%.c
		$(CC) -c $(CFLAGS) $< -o $@

else	# SWI

SHELL=/bin/bash
PLBASE=@PLBASE@
PLARCH=@PLARCH@
PL=@PL@
XPCEBASE=$(PLBASE)/xpce
PKGDOC=$(PLBASE)/doc/packages
PCEHOME=../xpce
LIBDIR=$(PLBASE)/library/http
EXDIR=$(PKGDOC)/examples/http

CC=gcc
COFLAGS=@COFLAGS@
CWFLAGS=@CWFLAGS@
CMFLAGS=@CMFLAGS@
CIFLAGS=
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H
LIBS=-lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl
LD=@LD@
LDFLAGS=@LDSOFLAGS@

PUBLICPL=swipl
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PUBLICPL) -q -s pltotex.pl -g pltotex --
RUNTEX=../../man/runtex
LATEX=latex
DOC=http
TEX=$(DOC).tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
HTML=$(DOC).html

endif

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

include $(srcdir)/common.mk

STREAMOBJ=	http_stream.o
JSONOBJ=	json.o
SOLIBS=		http_stream.so json.so

all:		$(SOLIBS)


http_stream.so:	$(STREAMOBJ)
		$(LD) $(LDFLAGS) -o $@ $(STREAMOBJ) $(LIBS)
json.so:	$(JSONOBJ)
		$(LD) $(LDFLAGS) -o $@ $(JSONOBJ) $(LIBS)

http_stream.o:	$(srcdir)/http_error.c $(srcdir)/http_chunked.c $(srcdir)/cgi_stream.c $(srcdir)/stream_range.c

install:	$(LIBPL) $(SOLIBS)
		mkdir -p $(BINTARGET)
		rm -f $(BINTARGET)/http_chunked.so
		$(INSTALL_PROGRAM) $(SOLIBS) $(BINTARGET)
		mkdir -p $(PLTARGET)
		mkdir -p $(PLTARGET)/web/icons
		mkdir -p $(DESTDIR)$(LIBDIR)/web/css
		$(INSTALL_DATA) $(LIBPL) $(PLTARGET)
		$(INSTALL_DATA) $(srcdir)/README $(PLTARGET)
		$(INSTALL_DATA) $(srcdir)/web/icons/*.png $(PLTARGET)/web/icons
		$(INSTALL_DATA) $(srcdir)/web/css/*.css $(DESTDIR)$(LIBDIR)/web/css
		$(PL) -f none -g make -t halt

ln-install:	$(LIBPL)
		$(MAKE) INSTALL_DATA="../ln-install" INSTALL_PROGRAM="../ln-install" install

rpm-install:	install

pdf-install:	install-examples
		$(INSTALL_DATA) $(DOC).pdf $(DESTDIR)$(PKGDOC)

html-install:	install-examples
		$(INSTALL_DATA) $(DOC).html httpserver.gif $(DESTDIR)$(PKGDOC)

install-examples::
		mkdir -p $(DESTDIR)$(EXDIR)
		mkdir -p $(DESTDIR)$(EXDIR)/pwp
		(cd examples && $(INSTALL_DATA) $(EXAMPLES) $(DESTDIR)$(EXDIR))
		(cd examples/pwp && $(INSTALL_DATA) *.pwp *.pl $(DESTDIR)$(EXDIR)/pwp)
		(cd examples && $(INSTALL_PROGRAM) $(EXAMPLEEXE) $(DESTDIR)$(EXDIR))

uninstall::
		(cd $(LIBDIR) && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

check::
		$(PL) -q -f $(srcdir)/test_http.pl -g test_http,halt -t 'halt(1)'
		$(PL) -q -f $(srcdir)/test_json.pl -g test_json,halt -t 'halt(1)'
		$(PL) -q -f $(srcdir)/test_cgi_stream.pl -g true -t test_cgi_stream

################################################################
# Documentation
################################################################

doc:		$(PDF) $(HTML)
pdf:		$(PDF)
html:		$(HTML)

SUBTEX=		post.tex json.tex httplog.tex httppath.tex htmlhead.tex \
		httpsession.tex httpdispatch.tex httpdirindex.tex httppwp.tex \
		httpopen.tex httphost.tex httpopenid.tex jswrite.tex

$(HTML):	$(TEX) $(SUBTEX)
		latex2html $(DOC)
		mv html/index.html $@
		mv html/httpserver.gif .
		rmdir html

$(PDF):		$(TEX) $(SUBTEX)
		$(RUNTEX) --pdf $(DOC)

$(TEX):		$(DOCTOTEX)

json.tex:	json.pl json_convert.pl http_json.pl

.txt.tex:
		$(PUBLICPL) -q -s txttotex.pl -g txttotex,halt -t 'halt(1)' -- $*.txt

httplog.tex:	http_log.pl
		$(PLTOTEX) --subsection 'library(http/http_log)'

httphost.tex:	http_host.pl
		$(PLTOTEX) --subsection 'library(http/http_host)'

httppath.tex:	http_path.pl
		$(PLTOTEX) --subsection 'library(http/http_path)'

httpopen.tex:	http_open.pl
		$(PLTOTEX) --subsection 'library(http/http_open)'

htmlhead.tex:	html_head.pl
		$(PLTOTEX) --subsection 'library(http/html_head)'

httpsession.tex: http_session.pl
		$(PLTOTEX) --subsection 'library(http/http_session)'

httpdispatch.tex: http_dispatch.pl
		$(PLTOTEX) --subsection 'library(http/http_dispatch)'

httpopenid.tex:	http_openid.pl
		$(PLTOTEX) --subsection 'library(http/http_openid)'

httpdirindex.tex: http_dirindex.pl
		$(PLTOTEX) --subsection 'library(http/http_dirindex)'

httppwp.tex:	http_pwp.pl
		$(PLTOTEX) --subsection 'library(http/http_pwp)'

jswrite.tex:	js_write.pl
		$(PLTOTEX) --subsection 'library(http/js_write)'

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex

################################################################
# Clean
################################################################

clean:
		rm -f $(STREAMOBJ) $(JSONOBJ)
		rm -f *~ *% config.log
		rm -f $(TEX) $(SUBTEX)
		$(RUNTEX) --clean $(DOC)
		rm -rf html

distclean:	clean
		rm -f $(SOLIBS) config.cache config.status config.h Makefile
