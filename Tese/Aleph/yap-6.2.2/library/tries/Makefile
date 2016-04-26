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
SHLIB_CFLAGS=-shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2
SO=so
#4.1VPATH=.:./OPTYap
CWD=$(PWD)
#

OBJS=core_tries.o base_tries.o tries.o base_itries.o itries.o
SOBJS=tries.so itries.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

core_tries.o: $(srcdir)/core_tries.h $(srcdir)/core_tries.c $(srcdir)/core_dbtries.h $(srcdir)/core_dbtries.c
	$(CC) -c $(CFLAGS) $(SHLIB_CFLAGS) $(srcdir)/core_tries.c -o core_tries.o

base_tries.o: $(srcdir)/core_tries.h $(srcdir)/base_tries.h $(srcdir)/base_tries.c $(srcdir)/core_dbtries.h $(srcdir)/base_dbtries.h $(srcdir)/base_dbtries.c
	$(CC) -c $(CFLAGS) $(SHLIB_CFLAGS) $(srcdir)/base_tries.c -o base_tries.o

tries.o: $(srcdir)/core_tries.h $(srcdir)/base_tries.h $(srcdir)/tries.c $(srcdir)/core_dbtries.h $(srcdir)/base_dbtries.h
	$(CC) -c $(CFLAGS) $(SHLIB_CFLAGS) $(srcdir)/tries.c -o tries.o

base_itries.o: $(srcdir)/core_tries.h $(srcdir)/base_itries.h $(srcdir)/base_itries.c
	$(CC) -c $(CFLAGS) $(SHLIB_CFLAGS) $(srcdir)/base_itries.c -o base_itries.o

itries.o: $(srcdir)/core_tries.h $(srcdir)/base_itries.h $(srcdir)/itries.c
	$(CC) -c $(CFLAGS) $(SHLIB_CFLAGS) $(srcdir)/itries.c -o itries.o

%.so: %.o
	gcc -shared -export-dynamic $(LDFLAGS) -o $@ $<  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

tries.so: core_tries.o base_tries.o tries.o
	gcc -shared -export-dynamic $(LDFLAGS) -o tries.so core_tries.o base_tries.o tries.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

itries.so: core_tries.o base_itries.o itries.o
	gcc -shared -export-dynamic $(LDFLAGS) -o itries.so core_tries.o base_itries.o itries.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK


