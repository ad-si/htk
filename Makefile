# NBNB - this Makefile should only be used with GNU make.

include top/mk/boilerplate.mk 
# boilerplate.mk should be included by all the Makefiles,
# and makes lib the default target.

# SUBDIRS is a list of all directories containing code to be compiled to
# form the UniForM workbench, in an order in which they can be compiled
# by recursively calling $(MAKE) -C (subdir) lib.
SUBDIRS=$(UTILDIR) $(CONCDIR) $(REACTORDIR) $(HTKDIR) $(DAVINCIDIR) $(WWWDIR)

unexport SUBDIRS

# TESTSUBDIRS is a list of directories (currently a subset of SUBDIRS
# though that may change) in which there is a subdirectory "test" in which
# tests can be compiled and run by $(MAKE) -C [testsubdir]/test all
TESTSUBDIRS=$(CONCDIR) $(REACTORDIR) $(HTKDIR) $(DAVINCIDIR)
unexport SUBDIRS

.PHONY : lib workbench testprg clean depend tarball.tar.gz

lib : workbench

workbench:
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) lib && ) echo Workbench done

testprg:
	$(foreach testsubdir,$(TESTSUBDIRS),$(MAKE) -C $(testsubdir)/test all && ) echo Tests done


depend:
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) depend && ) echo Depend done

tarball.tar.gz:
	gtar -czf tarball.tar.gz `gfind . '(' -name '*.hs' -o -name '*.c' -o -name '*.h' -o -name '*.mk' -o -name Makefile -o -name 'README' ')' ` 
