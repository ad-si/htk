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

# Options that are required when someone USES the libraries we've compiled
# should go into uni-package.options.
ifdef DEBUG
   HC_OPTIONS = -Onot -DDEBUG
else
    HC_OPTIONS = -O
endif

HCSHORTFLAGS = \
   -cpp -recomp -fwarn-deprecations \
   $(HC_OPTIONS) $(EXTRA_HC_OPTIONS) \
   -package-conf $(PACKAGECONF) 

PACKAGESARGS = $(PACKAGES:%=-package %)

HCFLAGS = $(HCSHORTFLAGS) $(PACKAGESARGS) 

THISPACKAGE = $(if $(PACKAGE),-package $(PACKAGE))

FIXFILENAMES = $(TOP)/mk/FixFileNames


# LINKFLAGS contains extra flags to be put at the end of the command line
# when compiling executables.
# LINKFLAGS = -lreadline
LINKFLAGS =

# Compiler for compiling C programs that are part of UniForM.  For the
# time being we use GHC.
CCH              = $(HC)
CFLAGS           = -package-conf $(PACKAGECONF) $(PACKAGESARGS)

# The package configuration directory
PACKAGECONF = $(GHCTOP)/uni-package.conf

# How to run cpp.  We do this via GHC.
CPP = $(HC) -E -cpp -I$(CINCLUDES) $(EXTRA_HC_OPTIONS)

# The directory containing C includes
CINCLUDES       = $(TOP)/includes


