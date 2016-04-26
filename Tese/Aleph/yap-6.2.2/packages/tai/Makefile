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
DEFS=-DHAVE_CONFIG_H -D_YAP_NOT_INSTALLED_=1
CC=gcc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I../.. -I$(srcdir)/../../include -I$(srcdir)/../PLStream
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

OBJS=pl-tai.o
SOBJS=pl-tai.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS) 

pl-tai.o: $(srcdir)/pl-tai.c
	(cd libtai ; $(MAKE))
	$(CC) -c $(CFLAGS) $(srcdir)/pl-tai.c -o pl-tai.o

pl-tai.so: pl-tai.o
	gcc -shared -export-dynamic $(LDFLAGS) -o pl-tai.so pl-tai.o libtai/libtai.a  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) -L$(abs_top_builddir)/packages/PLStream -lplstream

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK
	-(cd libtai && $(MAKE) clean)
