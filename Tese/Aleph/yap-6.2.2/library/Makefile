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
	$(srcdir)/INDEX.pl \
	$(srcdir)/apply.yap \
	$(srcdir)/apply_macros.yap \
	$(srcdir)/arg.yap \
	$(srcdir)/assoc.yap \
	$(srcdir)/atts.yap \
	$(srcdir)/autoloader.yap \
	$(srcdir)/avl.yap \
	$(srcdir)/bhash.yap \
	$(srcdir)/charsio.yap \
	$(srcdir)/cleanup.yap \
	$(srcdir)/clp/clpfd.pl \
	$(srcdir)/coinduction.yap \
	$(srcdir)/dbqueues.yap \
	$(srcdir)/dbusage.yap \
	$(srcdir)/dgraphs.yap \
	$(srcdir)/expand_macros.yap \
	$(srcdir)/gensym.yap \
	$(srcdir)/hacks.yap \
	$(srcdir)/heaps.yap \
	$(srcdir)/lambda.pl \
	$(srcdir)/lineutils.yap \
	$(srcdir)/lists.yap \
	$(srcdir)/nb.yap \
	$(srcdir)/ordsets.yap \
	$(srcdir)/maplist.yap \
	$(srcdir)/matlab.yap \
	$(srcdir)/matrix.yap \
	$(srcdir)/prandom.yap \
	$(srcdir)/queues.yap \
	$(srcdir)/random.yap \
	$(srcdir)/r_session.yap \
	$(srcdir)/rbtrees.yap \
	$(srcdir)/readutil.yap \
	$(srcdir)/regexp.yap \
	$(srcdir)/rltree.yap \
	$(srcdir)/splay.yap \
	$(srcdir)/stringutils.yap \
	$(srcdir)/system.yap \
	$(srcdir)/terms.yap \
	$(srcdir)/tries.yap \
	$(srcdir)/itries.yap \
	$(srcdir)/timeout.yap \
	$(srcdir)/trees.yap \
	$(srcdir)/ugraphs.yap \
	$(srcdir)/undgraphs.yap \
	$(srcdir)/varnumbers.yap \
	$(srcdir)/wdgraphs.yap \
	$(srcdir)/wgraphs.yap \
	$(srcdir)/wundgraphs.yap \
	$(srcdir)/lam_mpi.yap \
	$(srcdir)/ypp.yap \
	$(srcdir)/c_alarms.yap \
	$(srcdir)/flags.yap \
	$(srcdir)/block_diagram.yap

MYDDAS_PROGRAMS= $(srcdir)/MYDDAS/myddas.ypp \
	$(srcdir)/MYDDAS/myddas_assert_predicates.ypp \
	$(srcdir)/MYDDAS/myddas_mysql.ypp \
	$(srcdir)/MYDDAS/myddas_top_level.ypp \
	$(srcdir)/MYDDAS/myddas_errors.ypp \
	$(srcdir)/MYDDAS/myddas_prolog2sql.ypp \
	$(srcdir)/MYDDAS/myddas_util_predicates.ypp \
	$(srcdir)/MYDDAS/myddas_prolog2sql_optimizer.ypp

DIALECT_PROGRAMS= \
	$(srcdir)/dialect/commons.yap \
	$(srcdir)/dialect/hprolog.yap \
	$(srcdir)/dialect/swi.yap

DIALECT_SWI= \
	$(srcdir)/dialect/swi/INDEX.pl \
	$(srcdir)/dialect/swi/listing.pl \
	$(srcdir)/dialect/swi/readutil.pl


install: $(PROGRAMS) install_myddas
	mkdir -p $(DESTDIR)$(SHAREDIR)/Yap
	mkdir -p $(DESTDIR)$(SHAREDIR)/Yap/dialect
	mkdir -p $(DESTDIR)$(SHAREDIR)/Yap/dialect/swi
	for p in $(PROGRAMS); do $(INSTALL_DATA) $$p $(DESTDIR)$(SHAREDIR)/Yap; done
	for p in $(DIALECT_PROGRAMS); do $(INSTALL_DATA) $$p $(DESTDIR)$(SHAREDIR)/Yap/dialect; done
	for p in $(DIALECT_SWI); do $(INSTALL_DATA) $$p $(DESTDIR)$(SHAREDIR)/Yap/dialect/swi; done

install_myddas: $(MYDDAS_PROGRAMS)
	count=`echo "$(YAP_EXTRAS)" | grep MYDDAS | wc -l`; \
	if [ $$count -ne 0 ]; then \
	mkdir -p $(DESTDIR)$(SHAREDIR)/Yap; \
	for p in $(MYDDAS_PROGRAMS); \
	do $(INSTALL_DATA) $$p $(DESTDIR)$(SHAREDIR)/Yap; \
	done; \
	for p in $(DESTDIR)$(SHAREDIR)/Yap/myddas*.ypp; \
	do name=`basename $$p .ypp`; \
	cpp $(YAP_EXTRAS) -P -E -w $$p > $(DESTDIR)$(SHAREDIR)/Yap/$$name.yap; \
	rm -fr $$p; \
	done; \
	fi
