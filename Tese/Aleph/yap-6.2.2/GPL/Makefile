#
# default base directory for YAP installation
#
ROOTDIR = /usr/local
#
# where the binary should be
#
BINDIR = $(ROOTDIR)/bin
#
# where YAP should look for binary libraries
#
LIBDIR=${exec_prefix}/lib/Yap
#
# where YAP should look for architecture-independent Prolog libraries
#
SHAREDIR=$(ROOTDIR)/share
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
srcdir=.
YAP_EXTRAS= -DCOROUTINING=1 -DRATIONAL_TREES=1 -DDEPTH_LIMIT=1 

PROGRAMS= \
	$(srcdir)/aggregate.pl \
	$(srcdir)/ctypes.pl \
	$(srcdir)/error.pl \
	$(srcdir)/occurs.yap \
	$(srcdir)/pairs.pl


install: $(PROGRAMS)
	mkdir -p $(DESTDIR)$(SHAREDIR)/Yap
	for p in $(PROGRAMS); do $(INSTALL_DATA) $$p $(DESTDIR)$(SHAREDIR)/Yap; done

