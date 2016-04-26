#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
prefix = /usr/local
exec_prefix = ${prefix}
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
abs_top_builddir = /home/tiago/Tese/Tese/Aleph/yap-6.2.2
#
# where the binary should be
#
BINDIR = $(EROOTDIR)/bin
#
# where YAP should look for libraries
#
LIBDIR=${exec_prefix}/lib
YAPLIBDIR=${exec_prefix}/lib/Yap
#
#
CC=gcc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I../.. -I$(srcdir)/../../include
LDFLAGS=-rdynamic  -L$(abs_top_builddir) -Wl,-R$(LIBDIR) -Wl,-R,$(YAPLIBDIR)
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
SHELL=/bin/sh
RANLIB=ranlib
srcdir=.
SO=so
#4.1VPATH=.:./OPTYap
CWD=$(PWD)
#

OBJS=yap_random.o
SOBJS=yap_random.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS) 

yap_random.o: $(srcdir)/yap_random.c
	$(CC) -c $(CFLAGS) $(srcdir)/yap_random.c -o yap_random.o

%.so: %.o
	gcc -shared -export-dynamic $(LDFLAGS) -o $@ $<  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

yap_random.so: yap_random.o
	gcc -shared -export-dynamic $(LDFLAGS) -o yap_random.so yap_random.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK

