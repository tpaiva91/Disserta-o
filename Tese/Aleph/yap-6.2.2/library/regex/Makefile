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

OBJS=regexp.o # regcomp.o regexec.o regerror.o regfree.o
SOBJS=regexp.so  # regcomp.so regexec.so regerror.so regfree.so 

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

regexp.o: $(srcdir)/regexp.c  # $(srcdir)/regex2.h  $(srcdir)/engine.c
	$(CC) -c $(CFLAGS) $(srcdir)/regexp.c -o regexp.o

regcomp.o: $(srcdir)/regcomp.c $(srcdir)/regex2.h
	$(CC) -c $(CFLAGS) $(srcdir)/regcomp.c -o regcomp.o

regerror.o: $(srcdir)/regerror.c
	$(CC) -c $(CFLAGS) $(srcdir)/regerror.c -o regerror.o

regfree.o: $(srcdir)/regfree.c $(srcdir)/regex2.h
	$(CC) -c $(CFLAGS) $(srcdir)/regfree.c -o regfree.o

regexec.o: $(srcdir)/regexec.c
	$(CC) -c $(CFLAGS) $(srcdir)/regexec.c -o regexec.o

%.so: %.o
	gcc -shared -export-dynamic $(LDFLAGS) -o $@ $<   -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

regexp.so: regexp.o # regcomp.o regerror.o regfree.o regexec.o
	gcc -shared -export-dynamic $(LDFLAGS) -o regexp.so regexp.o   -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR) # regcomp.o regerror.o regfree.o regexec.o 

regcomp.so: regcomp.o # regfree.o
	gcc -shared -export-dynamic $(LDFLAGS) -o regcomp.so regcomp.o # regfree.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

regerror.so: regerror.o
	gcc -shared -export-dynamic $(LDFLAGS) -o regerror.so regerror.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

regfree.so: regfree.o
	gcc -shared -export-dynamic $(LDFLAGS) -o regfree.so regfree.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

regexec.so: regexec.o
	gcc -shared -export-dynamic $(LDFLAGS) -o regexec.so regexec.o  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK

