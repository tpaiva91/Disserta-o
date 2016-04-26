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

HEADERS=$(srcdir)/atoms.h $(srcdir)/pl-buffer.h $(srcdir)/pl-ctype.h  \
	$(srcdir)/pl-incl.h \
	$(srcdir)/pl-mswchar.h \
	$(srcdir)/pl-opts.h  \
	$(srcdir)/pl-os.h \
	$(srcdir)/pl-privitf.h \
	$(srcdir)/pl-stream.h \
	$(srcdir)/pl-table.h \
	$(srcdir)/pl-text.h $(srcdir)/pl-utf8.h \
	$(srcdir)/pl-yap.h # $(srcdir)/uxnt/dirent.h $(srcdir)/uxnt/utf8.h $(srcdir)/pl-utf8.c $(srcdir)/uxnt/uxnt.h
C_SOURCES=$(srcdir)/pl-buffer.c $(srcdir)/pl-ctype.c \
	$(srcdir)/pl-error.c  \
	$(srcdir)/pl-file.c $(srcdir)/pl-files.c  \
	$(srcdir)/pl-glob.c  \
	$(srcdir)/pl-os.c \
	$(srcdir)/pl-privitf.c \
	$(srcdir)/pl-stream.c $(srcdir)/pl-string.c \
	$(srcdir)/pl-table.c \
	$(srcdir)/pl-text.c \
	$(srcdir)/pl-util.c \
	$(srcdir)/pl-yap.c # $(srcdir)/popen.c $(srcdir)/uxnt/uxnt.c
OBJS=pl-buffer.o pl-ctype.o pl-error.o  \
	pl-file.o pl-files.o pl-glob.o pl-os.o pl-privitf.o \
	pl-stream.o pl-string.o pl-table.o \
	pl-text.o pl-util.o pl-utf8.o \
	pl-yap.o # uxnt.o
SOBJS=libplstream.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS) 

uxnt.o: $(srcdir)/uxnt/uxnt.c
	$(CC) -c $(CFLAGS) $(srcdir)/uxnt/uxnt.c -o uxnt.o

# default rule
%.o : $(srcdir)/%.c
	$(CC) -c $(CFLAGS) $< -o $@

%.so: %.o
	gcc -shared -export-dynamic $(LDFLAGS) -o $@ $<   -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

libplstream.so: $(OBJS)
	gcc -shared -export-dynamic $(LDFLAGS) -o libplstream.so $(OBJS)  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all
	$(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK


depend: $(HEADERS) $(C_SOURCES)
	-@if test "$(GCC)" = yes; then\
	  $(CC) -MM -MG $(CFLAGS) -I$(srcdir) -I$(srcdir)/../../include -I$(srcdir)/../../H $(C_SOURCES) >> Makefile;\
	else\
          makedepend -f - -- $(CFLAGS) -I$(srcdir)/../../H -I$(srcdir)/../../include -- $(C_SOURCES) |\
          sed 's|.*/\([^:]*\):|\1:|' >> Makefile ;\
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

