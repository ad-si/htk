# We establish the following conventions.
# Haskell files end with ".hs" or ".lhs", c with ".c".
# Source files are distinguished by prefix.
# Source files with names beginning "Test" are test files.
#    These are assumed to be linkable to complete executables
#    using only the libraries provided.  The resulting executable
#    has a name obtained by replacing "Test" with "test" and removing
#    the .hs prefix.  So "Test1.hs" goes to "test1".
#
# Source files with names beginning "Main" are main programs.
#    The linking rules are the same as for test files.  The name
#    of the resulting executable is obtained by removing "Main"
#    and the ".hs" file.  So "Mainserver.hs" goes to "server".
#
# All other files go into the library for the directory,
#    $(LIB), constructed out of the package name $(PACKAGE).
# The variables the Makefiles in the individual subdirectories
# need to set, if non-null, are
# SUBDIRS  the list of subdirectories to recurse to, which should
#             contain another Makefile and be in an order in which they
#             can be compiled.
# SRCS     all Haskell source files
# SRCSC    all C source files
# PACKAGE  the name of this package
# PACKAGES the name of packages we depend on (we only need direct
#          dependencies here).
# DOIMPORTS if set causes all .hi files to be copied to a subdirectory,
#          which should exist,
#          called imports, and entered in the package as being in that
#          directory.  This is necessary for all packages, like htk,
#          where the source files are not all in the head directory of
#          the package.
#
# BOOTSRCS Haskell source files which are sources for .hi-boot files.
#
# HTKLIBSUBDIRS Subdirectories which contain source files needed to
# compile libhtk.a.
# HTKNOTSRCS Files in SRCS which shouldn't be compiled for HTk.
#
# Other options which aren't used here (but in var.mk) and which
# can be useful:
#  
#    EXTRA_HC_OPTIONS any other options for GHC
#    
# These variables should be set before suffix.mk is read.
# The following targets are provided:
#
# depend   sets up dependencies between Haskell files 
#             (which may need to be done before anything else)
# libhere  makes the library file in this directory
# lib      makes the library files in this and all subdirectories
# testhere makes the test programs in this directory
# test     makes the test programs in this and all subdirectories
# all      makes both library and test programs in all subdirectories
# clean    cleans all compiled test programs, object files and libraries.
# display  displays Make environment variables
# displayhs displays all Haskell source files needed for libraries.
# displayhtkhs displays all Haskell source files needed for HTk.
#
# libfast  should be like lib but faster, since it uses ghc --make.

OBJSHS = $(patsubst %.hs,%.o,$(SRCS))
OBJSLHS = $(patsubst %.lhs,%.o,$(SRCSLHS))
OBJSALLHS = $(OBJSHS) $(OBJSLHS)
OBJSC = $(patsubst %.c,%.o,$(SRCSC))
OBJS = $(OBJSALLHS)  $(OBJSC)
LIBSRCS = $(filter-out Test%.hs Main%.hs,$(SRCS)) \
          $(filter-out Test%.lhs Main%.lhs,$(SRCSLHS))
HTKLIBSRCS = $(filter-out $(HTKNOTSRCS),$(LIBSRCS))
LIBOBJS = $(filter-out Test%.o Main%.o,$(OBJS))
TESTOBJS = $(filter Test%.o,$(OBJS))
TESTPROGS = $(patsubst Test%.o,test%,$(TESTOBJS))
MAINOBJS = $(filter Main%.o,$(OBJS))
MAINPROGS = $(patsubst Main%.o,%,$(MAINOBJS))

LIBOBJSHS = $(filter-out Test%.o Main%.o,$(OBJSHS))
TESTOBJSHS = $(filter Test%.o,$(OBJSHS))
MAINOBJSHS = $(filter Main%.o,$(OBJSHS))

LIBOBJSLHS = $(filter-out Test%.o Main%.o,$(OBJSLHS))
TESTOBJSLHS = $(filter Test%.o,$(OBJSLHS))
MAINOBJSLHS = $(filter Main%.o,$(OBJSLHS))

HILIBFILES = $(patsubst %.o,%.hi,$(LIBOBJSLHS) $(LIBOBJSHS))
HIFILES = $(patsubst %.o,%.hi,$(OBJSALLHS))
HIBOOTFILES = $(patsubst %.boot.hs,%.hi-boot,$(BOOTSRCS))

HSFILESALL = $(patsubst %.hs,$$PWD/%.hs,$(SRCS)) \
             $(patsubst %.lhs,$$PWD/%.lhs,$(SRCSLHS)) \
             $(patsubst %.boot.hs,$$PWD/%.boot.hs,$(BOOTSRCS))
# Can't be bothered to have a special variable for C header files.
# Instead we decree that all C files must have an associated header
# file.
OTHERSALL = $(patsubst %.c,$$PWD/%.c,$(SRCSC))  \
            $(patsubst %.c,$(CINCLUDES)/%.h,$(SRCSC)) \
            $$PWD/Makefile.in
ALLFILESALL = $(HSFILESALL) $(OTHERSALL)
#
#
# Constructing names for target files
# PACKAGE is the package name for this part of uni.  Normally it will
# begin with "uni-".
#
# LIB is the name of the containing library
LIB = lib$(PACKAGE).a
# GHCIOBJ is an object containing the whole library (needed for ghci).
GHCIOBJ = $(PACKAGE).o

# Handling the dependencies list
# This should be specified as PACKAGES, giving the PACKAGES we need.
# We only need direct dependencies.
# Is there any less horrible way of getting gmake to comma-separate
# a list?
DEPS' = $(filter-out BEGINCOMMA BEGIN,BEGIN$(PACKAGES:%=COMMA "%"))
DEPS = $(DEPS':COMMA=,)

#
#
# Here are some phony targets
#

# Specify that these targets don't correspond to files.
.PHONY : depend libhere lib testhere test all clean ghci libfast libfasthere displaysrcshere displayhshere displaysrcs displayhs displayhtkhshere displayhtkhs objsc objschere preparehtkwin packageherequick packagehere packages packagesquick

# The following gmake-3.77ism prevents gmake deleting all the
# object files once it has finished with them, so remakes
# actually work.
.SECONDARY : $(OBJS) $(HIFILES)

# all is the default target anyway, by virtual of boilerplate.mk.
all : packagehere mainhere testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) all && ) echo Finished make all

# ghci starts up GHC interactively.
ghci :
	$(HC) $(HCFLAGS)  --interactive
 
test : testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) test && ) echo Finished make test

main : mainhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) main && ) echo Finished make main

lib : libhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) lib && ) echo Finished make lib

libfast : libfasthere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) libfast && ) echo Finished make libfast

clean:
	$(RM) -f $(TESTPROGS) $(MAINPROGS) $(OBJS) $(HIBOOTFILES) $(HIFILES) $(LIB)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) clean && ) echo Finished make clean

display :
	@echo SRCS = $(SRCS)
	@echo SRCSLHS = $(SRCSLHS)
	@echo BOOTSRCS = $(BOOTSRCS)
	@echo SRCSC = $(SRCSC)
	@echo LIB = $(LIB)
	@echo LIBS = $(LIBS)
	@echo OBJSHS = $(OBJSHS)
	@echo OBJSLHS = $(OBJSLHS)
	@echo OBJSC = $(OBJSC)
	@echo OBJS = $(OBJS)
	@echo LIBOBJS = $(LIBOBJS)
	@echo TESTOBJS = $(TESTOBJS)
	@echo TESTPROGS = $(TESTPROGS)
	@echo MAINOBJS = $(MAINOBJS)
	@echo MAINPROGS = $(MAINPROGS)
	@echo HIFILES = $(HIFILES)
	@echo HIBOOTFILES = $(HIBOOTFILES)
	@echo HCDIRS = $(HCDIRS)
	@echo DEPS = '$(DEPS)'
	@echo PACKAGES = $(PACKAGES)
	@echo WINDOWS = $(WINDOWS)

depend : $(SRCS) $(SRCSLHS) $(HIBOOTFILES)
ifneq "$(strip $(SRCS) $(SRCSLHS))" ""
	$(DEPEND) $(HCFLAGS) $(SRCS) $(SRCSLHS)
endif
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) depend && ) echo Finished make depend

ifeq ($(LIBOBJS),)
# no library
   libhere :
else
   libhere : $(LIB)
endif

testhere : $(TESTPROGS)

mainhere : $(MAINPROGS)

libfasthere : $(OBJSC)
ifneq "$(strip $(PACKAGE))" ""
	$(HC) --make -package-name $(PACKAGE) $(HCFLAGS) $(HCLIBFLAGS) $(LIBSRCS)
	$(AR) -r $(LIB) $(LIBOBJS)
endif

$(GHCIOBJ) : $(LIB)
	$(LD) -r --whole-archive -o $@ $<


packagehere : libfasthere
	$(MAKE) packageherequick

packages : packagehere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packages && ) echo Finished make packages

packageherequick :
# The "echo" after the --remove-package means we keep going even if
# remove-package complains about the package not being there (which it won't
# be, the first time we use this).
ifneq "$(strip $(PACKAGE))" ""
	$(GHCPKG) --config-file $(PACKAGECONF) --remove-package $(PACKAGE) ; echo ""
ifneq "$(DOIMPORTS)" ""
# Copy import files over.
	$(RM) -rf imports
	$(MKDIR) imports
	$(CP) $(HILIBFILES) imports
endif
	sed -e 's+PACKAGE+$(PACKAGE)+g;s+IMPORTS+$(if $(DOIMPORTS),/imports)+g;s+DEPS+$(DEPS)+g' <$(TOP)/package.spec.template | $(FIXFILENAMES) | $(GHCPKG) --config-file $(PACKAGECONF) --add-package 
	$(MAKE) $(GHCIOBJ)
endif

packagesquick : packageherequick
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packagesquick && ) echo Finished make packagesquick


displaysrcshere :
	@PWD=`pwd`;echo $(ALLFILESALL)

displayhshere :
	@PWD=`pwd`;echo $(LIBSRCS)

displaysrcs : displaysrcshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) displaysrcs && ) echo 

displayhs : displayhshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) displayhs && ) echo


displayhtkhshere :
	@PWD=`pwd`;echo $(HTKLIBSRCS)

displayhtkhs : displayhtkhshere
	$(foreach subdir,$(HTKLIBSUBDIRS),$(MAKE) -r -C $(subdir) displayhtkhs &&) echo

objschere : $(OBJSC)
objsc : objschere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) objsc && ) echo

$(LIB) : $(LIBOBJS)
	$(RM) $@ ; $(AR) -r $@ $^

$(TESTPROGS) : test% :  Test%.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LINKFLAGS) $(THISPACKAGE)

$(MAINPROGS) : % :  Main%.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LINKFLAGS) $(THISPACKAGE)

$(HIFILES) : %.hi : %.o
	@:

$(HIBOOTFILES) : %.hi-boot : %.boot.hs
	$(RM) $@
	$(HC) $< $(HCFLAGS) $(HCLIBFLAGS) -no-recomp -c -o /dev/null -ohi $@

$(LIBOBJSHS) : %.o : %.hs
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS) $(HCLIBFLAGS)

$(LIBOBJSLHS) : %.o : %.lhs
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS) 

$(TESTOBJSHS) $(MAINOBJSHS) : %.o : %.hs
	$(HC) -c $< $(HCFLAGS) $(THISPACKAGE)

$(TESTOBJSLHS) $(MAINOBJSLHS) : %.o : %.lhs
	$(HC) -c $< $(HCFLAGS) $(THISPACKAGE)

# C objects ought to depend on the header file as well,
# but this is tricky when the C file is inside a subdirectory
# of this one.
$(OBJSC) : %.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

ifndef FAST
-include .depend
endif
# The dependencies file from the current directory.
# (trick stolen from GHC make files - setting FAST will sneakily
# prevent Make trying to recompile any of the dependent files.)




