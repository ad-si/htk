# This is the top-level directory for UniForM.  Like all the other
# makefiles in uni its main purpose is to set the variables required by
# mk/suffix.mk (see that for more details).  We also implement
# tarball.tar.gz which should gzip all source files to make tarball.tar.gz
#
# SUBDIRS is a list of all directories containing code to be compiled to
# form the UniForM workbench, in an order in which they can be compiled
# by recursively calling $(MAKE) -C (subdir) lib.
SUBDIRS=$(UTILDIR) $(CONCDIR) $(REACTORDIR) $(HTKDIR) $(DAVINCIDIR) $(WWWDIR) $(TOOLSDIR)

include top/mk/boilerplate.mk 

tarball.tar.gz:
	gtar -czf tarball.tar.gz `gfind . '(' -name '*.hs' -o -name '*.c' -o -name '*.h' -o -name '*.mk' -o -name Makefile -o -name 'README' ')' ` 



