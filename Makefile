# This is the top-level directory for UniForM.  Like all the other
# makefiles in uni its main purpose is to set the variables required by
# mk/suffix.mk (see that for more details).  We also implement
# tarball.tar.gz which should gzip all source files to make tarball.tar.gz
#
# SUBDIRS is a list of all directories containing code to be compiled to
# form the UniForM workbench, in an order in which they can be compiled
# by recursively calling $(MAKE) -C (subdir) lib.
SUBDIRS=$(UTILDIR) $(CONCDIR) $(SERVERDIR) $(REACTORDIR) $(HTKDIR) $(DAVINCIDIR) $(WWWDIR) $(TOOLSDIR) $(CVSDIR) $(VERSIONSDIR) 

include top/mk/boilerplate.mk 

# By making tarball.tar.gz and tarball.objects.tar.gz .PHONY we
# ensure that they always get remade whenever "gmake tarball.tar.gz"
# or "gmake tarball.objects.tar.gz" are executed.
.PHONY : tarball.tar.gz tarball.objects.tar.gz 

# tar of all source files
tarball.tar.gz:
	gtar -czf tarball.tar.gz `gfind . '(' -name '*.hs' -o -name '*.c' -o -name '*.h' -o -name '*.mk' -o -name Makefile -o -name 'README' ')' ` 

# tar of all object and .hi files (but not .depend/library/exec files which 
# are cheaper to regenerate)
tarball.objects.tar.gz:
	gtar -czf tarball.objects.tar.gz `gfind . '(' -name '*.o' -o -name '*.hi' ')' `

