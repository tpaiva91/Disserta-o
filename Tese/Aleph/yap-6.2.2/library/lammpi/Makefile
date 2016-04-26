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
CWD=$(PWD)
MPILDF=`$(MPI_CC) -showme|sed "s/[^ ]*//"|sed "s/-pt/-lpt/"`
MPICF=`$(MPI_CC) -showme| cut -d " " -f 2`
#

OBJS=yap_mpi.o hash.o prologterms2c.o
SOBJS=no
#yap_mpi.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

yap_mpi.o: $(srcdir)/yap_mpi.c   $(srcdir)/yap_mpi.c  
	$(MPI_CC)  $(CFLAGS) $(MPICF)  -c $(srcdir)/yap_mpi.c -o yap_mpi.o 

prologterms2c.o: $(srcdir)/prologterms2c.c $(srcdir)/prologterms2c.h
	$(CC) -c $(CFLAGS) $(srcdir)/prologterms2c.c -o prologterms2c.o

hash.o: $(srcdir)/hash.c $(srcdir)/hash.h
	$(CC) -c $(CFLAGS) $(srcdir)/hash.c -o hash.o

yap_mpi.so: $(OBJS)
	gcc -shared -export-dynamic  -o yap_mpi.so $(OBJS)   -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	@if test "$(SOBJS)" = "no"; then echo ""; else $(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR); fi

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK

no:
	@echo "YAP LAM/MPI module not compiled."

