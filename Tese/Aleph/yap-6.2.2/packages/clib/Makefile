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
LN_S=ln -s
#
#
DEFS=-DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1
CC=gcc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I../.. -I$(srcdir)/../../include -I. -I$(srcdir)/maildrop/rfc2045 -I$(srcdir)/maildrop/rfc822  
MKINDEX=true
NETLIBS=
CRYPTLIBS=-lcrypt -lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl
PTHREADLIBS=-lpthread

LD=	gcc -shared -export-dynamic
LDFLAGS= -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) -L$(abs_top_builddir)/packages/PLStream -lplstream -rdynamic  -L$(abs_top_builddir) -Wl,-R$(LIBDIR) -Wl,-R,$(YAPLIBDIR)

BINTARGET=$(DESTDIR)$(YAPLIBDIR)
PLTARGET=$(DESTDIR)$(SHAREDIR)

FINAL_BINTARGET=$(YAPLIBDIR)
FINAL_PLTARGET=$(SHAREDIR)

%.o: $(srcdir)/%.c
		$(CC) -c $(CFLAGS) $< -o $@

sha1/%.o: $(srcdir)/sha1/%.c
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
CIFLAGS=-I. -Imaildrop/rfc2045 -Imaildrop/rfc822
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H
LIBS=-lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl
NETLIBS=@NETLIBS@
CRYPTLIBS=@CRYPTLIBS@

PUBLICPL=swipl
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PUBLICPL) -q -s pltotex.pl -g pltotex --
DOC=clib
TEX=$(DOC).tex process.tex uri.tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
RUNTEX=../../man/runtex

LD=@LD@
LDFLAGS=@LDSOFLAGS@

BINTARGET=$(DESTDIR)$(PLBASE)/lib/$(PLARCH)
PLTARGET=$(DESTDIR)$(PLBASE)/library

FINAL_BINTARGET=$(PLBASE)/lib/$(PLARCH)
FINAL_PLTARGET=$(PLBASE)/library

endif	#YAP/SWI

MIMELIBS=maildrop/rfc2045/librfc2045.a maildrop/rfc822/librfc822.a

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

LIBPL=		$(srcdir)/unix.pl $(srcdir)/random.pl $(srcdir)/socket.pl $(srcdir)/cgi.pl $(srcdir)/memfile.pl $(srcdir)/filesex.pl $(srcdir)/mime.pl $(srcdir)/crypt.pl $(srcdir)/time.pl $(srcdir)/streaminfo.pl $(srcdir)/process.pl \
		$(srcdir)/prolog_server.pl $(srcdir)/sha.pl \
		$(srcdir)/socket.pl \
		$(srcdir)/uri.pl
TARGETS=	unix.so random.so socket.so cgi.so memfile.so files.so mime.so crypt.so time.so readutil.so streaminfo.so process.so \
		sha4pl.so uri.so
UNXOBJ=		error.o unix.o
SOCKOBJ=	error.o socket.o nonblockio.o
CGIOBJ=		error.o form.o cgi.o
CRYPTOBJ=	error.o crypt.o md5.o md5passwd.o # bsd-crypt.o
MEMOBJ=		error.o memfile.o
MIMEOBJ=	error.o mime.o
TIMEOBJ=	error.o time.o
FILESOBJ=	error.o files.o
RLIMITOBJ=	error.o rlimit.o
URIOBJ=		uri.o
RANDOMOBJ=	random.o
READUTILOBJ=	readutil.o
PROCESSOBJ=	error.o process.o
STREAMINFOOBJ=	error.o streaminfo.o
SHAOBJ=		error.o sha4pl.o sha1/sha1.o sha1/sha2.o \
		sha1/hmac_sha1.o sha1/hmac_sha256.o

all:		$(TARGETS)

random.so:	$(RANDOMOBJ)
		$(LD) $(LDFLAGS) -o $@ $(RANDOMOBJ)
unix.so:	$(UNXOBJ)
		$(LD) $(LDFLAGS) -o $@ $(UNXOBJ)
socket.so:	$(SOCKOBJ)
		$(LD) $(LDFLAGS) -o $@ $(SOCKOBJ) $(NETLIBS) $(PTHREADLIBS)
cgi.so:	$(CGIOBJ)
		$(LD) $(LDFLAGS) -o $@ $(CGIOBJ) $(PTHREADLIBS)
crypt.so:	$(CRYPTOBJ)
		$(LD) $(LDFLAGS) -o $@ $(CRYPTOBJ) $(CRYPTLIBS) $(PTHREADLIBS)
memfile.so:	$(MEMOBJ)
		$(LD) $(LDFLAGS) -o $@ $(MEMOBJ) $(LIBS)
mime.so:	$(MIMEOBJ) $(MIMELIBS)
		$(LD) $(LDFLAGS) -o $@ $(MIMEOBJ) $(MIMELIBS)
time.so:	$(TIMEOBJ)
		$(LD) $(LDFLAGS) -o $@ $(TIMEOBJ) $(LIBS) $(PTHREADLIBS)
files.so:	$(FILESOBJ)
		$(LD) $(LDFLAGS) -o $@ $(FILESOBJ) $(LIBS)
rlimit.so:	$(RLIMITOBJ)
		$(LD) $(LDFLAGS) -o $@ $(RLIMITOBJ) $(LIBS)
readutil.so:	$(READUTILOBJ)
		$(LD) $(LDFLAGS) -o $@ $(READUTILOBJ)
process.so:	$(PROCESSOBJ)
		$(LD) $(LDFLAGS) -o $@ $(PROCESSOBJ)
streaminfo.so: $(STREAMINFOOBJ)
		$(LD) $(LDFLAGS) -o $@ $(STREAMINFOOBJ)
sha4pl.so:	$(SHAOBJ)
		$(LD) $(LDFLAGS) -o $@ $(SHAOBJ)
uri.so:	$(URIOBJ)
		$(LD) $(LDFLAGS) -o $@ $(URIOBJ) $(PTHREADLIBS)

socket.o:	$(srcdir)/socket.c $(srcdir)/sockcommon.c

sha1/hmac_sha1.o:
		$(CC) -c $(CFLAGS) -DUSE_SHA1 -o $@ $(srcdir)/sha1/hmac.c
sha1/hmac_sha256.o:
		$(CC) -c $(CFLAGS) -DUSE_SHA256 -o $@ $(srcdir)/sha1/hmac.c

maildrop/rfc822/librfc822.a:
		(cd maildrop/rfc822 && $(MAKE))
maildrop/rfc2045/librfc2045.a:
		(cd maildrop/rfc2045 && $(MAKE))


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
		@$(MAKE) INSTALL_DATA='../ln-install' INSTALL_PROGRAM='../ln-install' install

rpm-install:	install

html-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 clib.html $(DESTDIR)$(PKGDOC)

pdf-install::
		mkdir -p $(DESTDIR)$(PKGDOC)
		$(INSTALL) -m 644 clib.pdf $(DESTDIR)$(PKGDOC)

uninstall::
		(cd $(FINAL_BINTARGET) && rm -f $(TARGETS))
		(cd $(FINAL_PLTARGET) && rm -f $(LIBPL))
		$(PL) -f none -g make -t halt

################################################################
# Testing
################################################################

check:		check-socket check-memfile check-unit check-process \
		check-read check-uri check-cgi

torture:	torture-socket

check-socket::
		$(PL) -q -f $(srcdir)/test_socket.pl -F none -g test_socket,halt -t 'halt(1)'

torture-socket::
		$(PL) -q -f $(srcdir)/stresssocket.pl -F none -g test,halt -t 'halt(1)'

check-memfile::
		$(PL) -q -f $(srcdir)/test_memfile.pl -F none -g test_memfile,halt -t 'halt(1)'

check-unit::
		$(PL) -q -f $(srcdir)/test.pl -g run_tests,halt -t 'halt(1)'

check-uri::
		$(PL) -q -f $(srcdir)/test_uri.pl -g true -t test_uri

check-cgi::
		$(PL) -q -f $(srcdir)/test_cgi.pl -g true -t test_cgi

check-process::
		$(PL) -q -f $(srcdir)/test_process.pl -g true -t test_process

check-read::
		$(PL) -q -f $(srcdir)/test_readutil.pl -g true -t test_readutil

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
		mv html/index.html clib.html
		rm -r html

$(TEX):		$(DOCTOTEX)

uri.tex:	uri.pl
		$(PLTOTEX) 'library(uri)'
process.tex:	process.pl
		$(PLTOTEX) 'library(process)'

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex


################################################################
# Clean
################################################################

clean:
		rm -f $(OBJ) *~ *.o *% a.out core config.log sha1/*.o
		rm -f $(TEX)
		-(cd maildrop/rfc822 && $(MAKE) clean)
		-(cd maildrop/rfc2045 && $(MAKE) clean)

distclean:	clean
		rm -f $(TARGETS) config.cache config.h config.status Makefile
		rm -f clib.aux clib.log clib.out clib.toc
		rm -rf html
		-(cd maildrop/rfc822 && $(MAKE) distclean)
		-(cd maildrop/rfc2045 && $(MAKE) distclean)
