#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
prefix = /usr/local
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
#
# where the binary should be
#
BINDIR = $(EROOTDIR)/bin
#
# where YAP should look for libraries
#
LIBDIR=$(EROOTDIR)/lib/Yap
DESTDIR=$(prefix)/share/Yap
#
#
CC=gcc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS)  -I$(srcdir)/../../../include  
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
SHELL=/bin/sh
RANLIB=ranlib
SHLIB_CFLAGS=-shared -fPIC
SHLIB_SUFFIX=.so
CWD=$(PWD)
srcdir=.
CPLINT_CFLAGS=
CPLINT_LDFLAGS=
CPLINT_LIBS=

#


default:  bddem.so

bddem.so: bddem.o
	$(CC) -shared -export-dynamic  $(CPLINT_LDFLAGS) bddem.o  $(CPLINT_LIBS) -o bddem.so  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

bddem.o : $(srcdir)/bddem.c
	$(CC) -c $(CFLAGS) $(CPLINT_CFLAGS)  $(srcdir)/bddem.c -o bddem.o

clean: 
	rm -f *.o bddem.so

install: default
	$(INSTALL_PROGRAM) bddem.so $(DESTDIR)
