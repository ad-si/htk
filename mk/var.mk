
# Variable Section
# 
# Export all variables!
# export 

include top/mk/machinedep.mk # set variables which depend on the machine
-include top/mk/local.mk  # If available, include local variable changes
# (This file is not included in the standard CVS distribution and
# is intended to contain directives for debugging.)

# Glasgow Haskell Compiler
# HCHOME set in machinedep.mk
HC               = $(HCHOME)/bin/ghc
DEPEND           = $(HCHOME)/bin/ghc -M -optdep-f -optdep.depend
# Meaning of ghc -M options (extracted from GHC Make files)
# (1) -optdep.depend makes dependences go into .depend files.
# (2) -f means .depend doesn't have to exist first for this to work.
GHCINCDIR        = -I$(HCHOME)/lib/includes 

ifdef DEBUG
   HC_OPTIONS = -recomp -Onot -DDEBUG
else
   HC_OPTIONS = -recomp -O -O2-for-C
endif

HCSYSLIBS = -syslib concurrent -syslib data -syslib net -syslib posix -syslib text -syslib util -syslib lang
# Version as it used to be before the GHC library names all changed
# HCSYSLIBS = -syslib posix -syslib misc -syslib exts -syslib concurrent

HCLIBSACTUAL = $(HCHOME)/lib/libHSposix.a $(HCHOME)/lib/libHSposix_cbits.a $(HCHOME)/lib/libHSmisc.a $(HCHOME)/lib/libHSmisc_cbits.a $(HCHOME)/lib/libHSexts.a $(HCHOME)/lib/libHSconcurrent.a $(HCHOME)/lib/libHS.a $(HCHOME)/lib/libHS_cbits.a $(HCHOME)/lib/libHSrts.a $(HCHOME)/lib/libgmp.a -lc -lm

HCFLAGS = $(HCSYSLIBS) \
          -i$(HCDIRS) \
          -fglasgow-exts \
          -fallow-overlapping-instances \
          -fallow-undecidable-instances \
	  -cpp -hi-diffs -fvia-C \
	  $(GHCINCDIR) \
	  -H25M -K5M $(HC_OPTIONS) $(EXTRA_HC_OPTIONS)

# Gnu C compiler.  NB - the GHC installation is hardwired to
# a particular version of gcc, so don't go changing this unless
# you change the GHC installation.  
CC               = gcc
CFLAGS           = $(GHCINCDIR)

# Subdirectories
# . is automatically included by mkdependHS anyway (I don't
# know how to turn it off).  So why put it at the start of
# HCDIRS when that gives lots of extra warnings?  The reason
# is so that mkdependHS uses the local name for files in the
# same directory, so that gmake properly identifies them.
HCDIRS =  .:$(UTILDIR):$(CONCDIR):$(REACTORDIR):$(HTKDIR):$(HTKRESOURCEDIR):$(HTKCANVASITEMDIR):$(HTKKERNELDIR):$(HTKCONTAINERDIR):$(HTKMENUITEMDIR):$(HTKTOOLKITDIR):$(HTKCOMPONENTSDIR):$(HTKTOPLEVELDIR):$(HTKWIDGETSDIR):$(HTKDEVICESDIR):$(HTKTEXTITEMDIR):$(DAVINCIDIR):$(WWWDIR):$(TOOLSDIR)
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
TOOLSDIR         = top/tools


