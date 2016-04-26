################################################################
# Makefile for librfc822.a
#
# Author: Jan Wielemaker (wielemak@science.uva.nl)
################################################################

CC=gcc
CFLAGS=-O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 -DHAVE_CONFIG_H
LIBS=-lresolv -lnss_dns -lnss_files -lcrypt -lstdc++ -lm  -lreadline -lncurses -lz -ldl -lnsl
AR=ar
RANLIB=ranlib

ifeq (yap,yap)

srcdir=.

SHLIB_CFLAGS=-shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2

%.o: $(srcdir)/%.c
		$(CC) -c $(CFLAGS)  -I../../../..  $(SHLIB_CFLAGS) $< -o $@
endif

OBJ=	rfc822.o rfc822_getaddr.o rfc822_getaddrs.o \
	rfc822_mkdate.o rfc822_parsedt.o rfc2047u.o \
	rfc2047.o imapsubj.o imaprefs.o

all:	librfc822.a

librfc822.a: $(OBJ)
	rm -f librfc822.a
	$(AR) r librfc822.a $(OBJ)
	$(RANLIB) librfc822.a


################################################################
# Clean
################################################################

clean:
		rm -f $(OBJ) *~ *.o *% a.out core config.log librfc822.a

distclean:	clean
		rm -f Makefile config.status config.h librfc822.a
		rm -rf autom4te.cache
		rm -f stamp-h1
