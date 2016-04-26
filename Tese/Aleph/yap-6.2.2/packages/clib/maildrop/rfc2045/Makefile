################################################################
# Makefile for librfc2045.a
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
		$(CC) -c $(CFLAGS) -I$(srcdir) -I$(srcdir)/.. -I../../../.. $(SHLIB_CFLAGS) $< -o $@
endif

OBJ=	rfc2045.o rfc2045acchk.o rfc2045acprep.o \
	rfc2045appendurl.o rfc2045cdecode.o rfc2045decode.o \
	rfc2045enomem.o \
	rfc2045_base64encode.o rfc2045_fromfd.o \
	rfc2045find.o rfc2045mkboundary.o rfc2045rewrite.o \
	rfc2045tryboundary.o rfc2045xdump.o

all:	librfc2045.a

librfc2045.a: $(OBJ)
	rm -f librfc2045.a
	$(AR) r librfc2045.a $(OBJ)
	$(RANLIB) librfc2045.a


################################################################
# Clean
################################################################

clean:
		rm -f $(OBJ) *~ *.o *% a.out core config.log librfc2045.a

distclean:	clean
		rm -f Makefile config.status config.h librfc2045.a
		rm -f rfc2045charset.h stamp-h1
		rm -rf autom4te.cache
