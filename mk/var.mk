
# Variable Section
# 
# Export all variables!
# export 
include top/mk/settings.mk # find out location of top directory


include $(UNIDIR)/mk/machinedep.mk # set variables which depend on the machine
-include $(UNIDIR)/mk/local.mk  # If available, include local variable changes
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
   HC_OPTIONS = -recomp -fwarn-deprecations -Onot -DDEBUG
else
   HC_OPTIONS = -recomp -fwarn-deprecations -O -O2-for-C
endif

HCSYSLIBS = -package concurrent -package data -package net -package posix -package text -package util -package lang

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
# NB.  XHCDIRS can be used to add extra directories.  It should
# end with a colon.
HCDIRS =  .:$(XHCDIRS)$(UTILDIR):$(CONCDIR):$(REACTORDIR):$(HTKDIR):$(DAVINCIDIR):$(WWWDIR):$(TOOLSDIR):$(CVSDIR):$(VERSIONSDIR)
# $(OMSCDIR):$(OMSNOTIDIR):$(OMSDIR):$(WBDIR):$(TOOLSDIR):$(DEMODIR):$(SCHEMADIR):$(IDLDIR)

UTILDIR    	= $(UNIDIR)/util
CONCDIR    	= $(UNIDIR)/concurrency
REACTORDIR 	= $(UNIDIR)/reactor
HTKDIR          = $(UNIDIR)/htk
HTKRESOURCEDIR  = $(HTKDIR)/resources
HTKCANVASITEMDIR= $(HTKDIR)/canvasitems
HTKKERNELDIR    = $(HTKDIR)/kernel
HTKCONTAINERDIR = $(HTKDIR)/containers
HTKMENUITEMDIR  = $(HTKDIR)/menuitems
HTKTOOLKITDIR   = $(HTKDIR)/toolkit
HTKCOMPONENTSDIR= $(HTKDIR)/components
HTKTOPLEVELDIR  = $(HTKDIR)/toplevel
HTKWIDGETSDIR   = $(HTKDIR)/widgets
HTKDEVICESDIR   = $(HTKDIR)/devices
HTKTEXTITEMDIR  = $(HTKDIR)/textitems
DAVINCIDIR      = $(UNIDIR)/davinci
WWWDIR          = $(UNIDIR)/www
TOOLSDIR        = $(UNIDIR)/tools
CVSDIR          = $(UNIDIR)/cvs
VERSIONSDIR     = $(UNIDIR)/versions
VERSIONSINODEDIR= $(CVSDIR)/inodeserver


# HTKDIRS and HTKSDIRS contain the HTK .hi files.  HTKSDIR gives the
# view from the htk directory.
HTKDIRS = $(HTKRESOURCEDIR):$(HTKCANVASITEMDIR):$(HTKKERNELDIR):$(HTKCONTAINERDIR):$(HTKMENUITEMDIR):$(HTKTOOLKITDIR):$(HTKCOMPONENTSDIR):$(HTKTOPLEVELDIR):$(HTKWIDGETSDIR):$(HTKDEVICESDIR):$(HTKTEXTITEMDIR):

HTKSDIRS = resources:canvasitems:kernel:containers:menuitems:toolkit:components:toplevel:widgets:devices:textitems:


