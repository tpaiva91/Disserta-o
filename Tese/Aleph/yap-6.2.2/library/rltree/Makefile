#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
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
MPI_CC=mpicc
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
#

OBJS=yaprl.o range_list.o
SOBJS=yap_rl.so

#in some systems we just create a single object, in others we need to
# create a libray
all: $(SOBJS)

range_list.o: $(srcdir)/range_list.c $(srcdir)/range_list.h
	$(CC) -c $(CFLAGS) $(srcdir)/range_list.c -o range_list.o

yaprl.o: $(srcdir)/yap_rl.c
	$(CC) -c $(CFLAGS) $(srcdir)/yap_rl.c -o yaprl.o

%.so: %.o
	gcc -shared -export-dynamic $(LDFLAGS) -o $@ $<  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

yap_rl.so: $(OBJS)
	gcc -shared -export-dynamic $(LDFLAGS) -o yap_rl.so $(OBJS)  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	@if test "$(SOBJS)" = "no"; then echo ""; else $(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR); fi

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK

no:
	@echo "YAP RL module not compiled."

