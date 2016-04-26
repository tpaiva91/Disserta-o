#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
GCC=yes
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
CXX=g++
CXXFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I../../.. -I$(srcdir)/../../../include  
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

HEADERS= \
	$(srcdir)/Alg.h \
	$(srcdir)/BasicHeap.h \
	$(srcdir)/BoxedVec.h  \
	$(srcdir)/Heap.h \
	$(srcdir)/Map.h \
	$(srcdir)/Queue.h  \
	$(srcdir)/Solver.h \
	$(srcdir)/SolverTypes.h \
	$(srcdir)/Sort.h \
	$(srcdir)/Vec.h
C_SOURCES=$(srcdir)/pl-minisat.C $(srcdir)/Solver.C

OBJS = \
	Solver.o \
	pl-minisat.o

SOBJS=pl-minisat.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS) 

# default rule
Solver.o : $(srcdir)/Solver.C
	$(CXX) -c $(CXXFLAGS) $(srcdir)/Solver.C -o Solver.o

pl-minisat.o : $(srcdir)/pl-minisat.C
	$(CXX) -c $(CXXFLAGS) $(srcdir)/pl-minisat.C -o pl-minisat.o

pl-minisat.so: $(OBJS)
	g++ -shared -export-dynamic $(LDFLAGS) -o pl-minisat.so $(OBJS)  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) -L$(abs_top_builddir)/packages/PLStream -lplstream

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK


depend: $(HEADERS) $(C_SOURCES)
	-@if test "$(GCC)" = yes; then\
	  $(CC) -MM -MG $(CFLAGS) -I$(srcdir) -I$(srcdir)/../../../include -I$(srcdir)/../../../H $(C_SOURCES) >> Makefile;\
	else\
          makedepend -f - -- $(CFLAGS) -I$(srcdir)/../../../H -I$(srcdir)/../../../include -- $(C_SOURCES) |\
          sed 's|.*/\([^:]*\):|\1:|' >> Makefile ;\
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.
