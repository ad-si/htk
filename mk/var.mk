
# Variable Section
# 
# Export all variables!
# export 

include top/mk/machinedep.mk # set variables which depend on the machine

# Glasgow Haskell Compiler
# HCHOME set in machinedep.mk
HC               = $(HCHOME)/bin/ghc
DEPEND           = $(HCHOME)/bin/ghc -M -optdep
# GHCINCDIR        = -I$(HCHOME)/lib/includes -I/usr/local/lang/gnu/lib/gcc-lib/sparc-sun-solaris2.6/2.7.2.3/include
GHCINCDIR        = -I$(HCHOME)/lib/includes 
EXTRA_HC_OPTIONS = -recomp -Onot

HCSYSLIBS = -syslib concurrent -syslib data -syslib net -syslib posix -syslib text -syslib util -syslib lang
# Version as it used to be before the GHC library names all changed
# HCSYSLIBS = -syslib posix -syslib misc -syslib exts -syslib concurrent

HCLIBSACTUAL = $(HCHOME)/lib/libHSposix.a $(HCHOME)/lib/libHSposix_cbits.a $(HCHOME)/lib/libHSmisc.a $(HCHOME)/lib/libHSmisc_cbits.a $(HCHOME)/lib/libHSexts.a $(HCHOME)/lib/libHSconcurrent.a $(HCHOME)/lib/libHS.a $(HCHOME)/lib/libHS_cbits.a $(HCHOME)/lib/libHSrts.a $(HCHOME)/lib/libgmp.a -lc -lm

HCFLAGS = $(HCSYSLIBS) \
          -i$(HCDIRS) \
          -fglasgow-exts \
          -fallow-undecidable-instances \
          -fallow-overlapping-instances \
	  -cpp -hi-diffs -fvia-C \
	  $(GHCINCDIR) \
	  -H25M -K5M $(EXTRA_HC_OPTIONS) $(HCDEFS) $(HCINC)

# Gnu C compiler.  NB - the GHC installation is hardwired to
# a particular version of gcc, so don't go changing this unless
# you change the GHC installation.  
CC               = gcc
CFLAGS           = $(GHCINCDIR)

# mustmake script.  gmake always assume a file has been touched
# when it is remade, without checking the date stamp.  This is
# bad for example for .hi files.  So we use the mustmake script
# to explicitly check the dependencies.
MUSTMAKE = top/bin/mustmake

# Subdirectories
# . is automatically included by mkdependHS anyway (I don't
# know how to turn it off).  So why put it at the start of
# HCDIRS when that gives lots of extra warnings?  The reason
# is so that mkdependHS uses the local name for files in the
# same directory, so that gmake properly identifies them.
HCDIRS =  .:$(UTILDIR):$(CONCDIR):$(REACTORDIR):$(HTKDIR):$(HTKRESOURCEDIR):$(HTKCANVASITEMDIR):$(HTKKERNELDIR):$(HTKCONTAINERDIR):$(HTKMENUITEMDIR):$(HTKTOOLKITDIR):$(HTKCOMPONENTSDIR):$(HTKTOPLEVELDIR):$(HTKWIDGETSDIR):$(HTKDEVICESDIR):$(HTKTEXTITEMDIR):$(DAVINCIDIR):$(WWWDIR)
# $(OMSCDIR):$(OMSNOTIDIR):$(OMSDIR):$(WBDIR):$(TOOLSDIR):$(DEMODIR):$(SCHEMADIR):$(IDLDIR)

UTILDIR    	= top/util
CONCDIR    	= top/concurrency
REACTORDIR 	= top/reactor
HTKDIR           = top/htk
HTKRESOURCEDIR    =  $(HTKDIR)/resources
HTKCANVASITEMDIR    = $(HTKDIR)/canvasitems
HTKKERNELDIR     = $(HTKDIR)/kernel
HTKCONTAINERDIR   = $(HTKDIR)/containers
HTKMENUITEMDIR   = $(HTKDIR)/menuitems
HTKTOOLKITDIR    = $(HTKDIR)/toolkit
HTKCOMPONENTSDIR = $(HTKDIR)/components
HTKTOPLEVELDIR   = $(HTKDIR)/toplevel
HTKWIDGETSDIR    = $(HTKDIR)/widgets
HTKDEVICESDIR    = $(HTKDIR)/devices
HTKTEXTITEMDIR   = $(HTKDIR)/textitems
DAVINCIDIR       = top/davinci
WWWDIR           = top/www


