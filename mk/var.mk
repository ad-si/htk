# Variable Section
# 
-include $(TOP)/mk/local.mk  # If available, include local variable changes
# (This file is not included in the standard CVS distribution and
# is intended to contain directives for debugging.)

# Includes for local variable changes at the end of this file

# Glasgow Haskell Compiler
# HC is set in machinedep.mk
DEPEND           = $(HC) -M -optdep-f -optdep.depend
# Meaning of ghc -M options (extracted from GHC Make files)
# (1) -optdep.depend makes dependences go into .depend files.
# (2) -f means .depend doesn't have to exist first for this to work.

ifdef DEBUG
   HC_OPTIONS = -recomp -fwarn-deprecations -Onot -DDEBUG
else
#    HC_OPTIONS = -recomp -fwarn-deprecations -O -fasm
    HC_OPTIONS = -recomp -fwarn-deprecations -O -O2-for-C
endif

HCSYSLIBS = -package concurrent -package data -package net -package posix -package text -package util -package lang

HCSHORTFLAGS = \
   $(HCSYSLIBS) -fglasgow-exts \
   -fallow-overlapping-instances -fallow-undecidable-instances \
   -cpp -ddump-hi-diffs -H25M $(HC_OPTIONS) $(EXTRA_HC_OPTIONS)

HCFLAGS = -i$(HCDIRS) $(HCSHORTFLAGS)

# LINKFLAGS contains extra flags to be put at the end of the command line
# when compiling executables.
LINKFLAGS = -lreadline

# Gnu C compiler.  NB - the GHC installation is hardwired to
# a particular version of gcc, so don't go changing this unless
# you change the GHC installation.  
CC               = $(HC)
CFLAGS           = $(GHCINCDIR)

# Subdirectories
# . is automatically included by mkdependHS anyway (I don't
# know how to turn it off).  So why put it at the start of
# HCDIRS when that gives lots of extra warnings?  The reason
# is so that mkdependHS uses the local name for files in the
# same directory, so that gmake properly identifies them.
# NB.  XHCDIRS can be used to add extra directories.  It should
# end with a colon.
HCDIRS =  .:$(XHCDIRS)$(UTILDIR):$(EVENTSDIR):$(REACTORDIR):$(SERVERDIR):$(HTKDIR):$(DAVINCIDIR):$(GRAPHSDIR):$(TOOLSDIR):$(CVSDIR):$(TYPESDIR)
# $(OMSCDIR):$(OMSNOTIDIR):$(OMSDIR):$(WBDIR):$(TOOLSDIR):$(DEMODIR):$(SCHEMADIR):$(IDLDIR)

UTILDIR    	= $(TOP)/util
EVENTSDIR       = $(TOP)/events
REACTORDIR 	= $(TOP)/reactor
SERVERDIR       = $(TOP)/server
HTKDIR          = $(TOP)/htk
HTKPACKERDIR    = $(HTKDIR)/packer
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
HTKTIXDIR       = $(HTKDIR)/tix
DAVINCIDIR      = $(TOP)/davinci
GRAPHSDIR       = $(TOP)/graphs
TOOLSDIR        = $(TOP)/tools
CVSDIR          = $(TOP)/cvs
TYPESDIR        = $(TOP)/types
VERSIONSDIR     = $(TOP)/versions
VERSIONSINODEDIR= $(CVSDIR)/inodeserver


# HTKDIRS and HTKSDIRS contain the HTK .hi files.  HTKSDIR gives the
# view from the htk directory.
HTKDIRS = $(HTKPACKERDIR):$(HTKRESOURCEDIR):$(HTKCANVASITEMDIR):$(HTKKERNELDIR):$(HTKCONTAINERDIR):$(HTKMENUITEMDIR):$(HTKTOOLKITDIR):$(HTKCOMPONENTSDIR):$(HTKTOPLEVELDIR):$(HTKWIDGETSDIR):$(HTKDEVICESDIR):$(HTKTEXTITEMDIR):$(HTKTIXDIR):

HTKSDIRS = packer:resources:canvasitems:kernel:containers:menuitems:toolkit:components:toplevel:widgets:devices:textitems:tix:

# COBJS is used for ghci.
COBJS = $(CVSDIR)/copy_file.o $(UTILDIR)/object.o $(UTILDIR)/default_options.o 
