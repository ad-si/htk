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
#    $(LIB), constructed out of the package name $PACKAGE).
# The variables the Makefiles in the individual subdirectories
# need to set, if non-null, are
# SUBDIRS  the list of subdirectories to recurse to, which should
#             contain another Makefile and be in an order in which they
#             can be compiled.
# SRCS     all Haskell source files
# SRCSC    all C source files
# SRCSEMACS all Emacs Lisp files
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
# EXTRAEXPORTS Files in this directory which should be exported (in binary
# distributions), in addition to those which are automatically picked up.
#
# Other options which aren't used here (but in var.mk) and which
# can be useful:
#  
#    EXTRA_HC_OPTIONS any other options for GHC
#    
# These variables should be set before suffix.mk is read.
# The following targets are provided:
#
# There is another more detailed list of targets in uni/MAKE.TARGETS.
# 
# boot     does set
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
#
# libfast  should be like lib but faster, since it uses ghc --make.
#
# exports  construct .zip and .tar.gz binary archives.
#
# We also define trivial targets doc and dochere.  The subdirectory
# Makefiles should implement appropriate dependencies for dochere, if any!!
# These dependencies should make the documentation
# 
# Likewise we define www, wwwtest and wwwhere.  
# gmake www should copy all the Web pages over onto the web-site.  The location
#    of the web-site should be specified in the variable WWWPREFIX; if it
#    is empty gmake www will complain.  The individual Makefiles should
#    declare dependencies for wwwhere where necessary to do the copying.

OBJSHS = $(patsubst %.hs,%.o,$(SRCS))
OBJSLHS = $(patsubst %.lhs,%.o,$(SRCSLHS))
OBJSALLHS = $(OBJSHS) $(OBJSLHS)
OBJSC = $(patsubst %.c,%.o,$(SRCSC))
OBJSEMACS = $(patsubst %.el,%.elc,$(SRCSEMACS))
OBJS = $(OBJSALLHS)  $(OBJSC)
LIBSRCS = $(filter-out Test%.hs Main%.hs,$(SRCS)) \
          $(filter-out Test%.lhs Main%.lhs,$(SRCSLHS))
EXPORTSRCS = $(filter Test%.hs Main%.hs Test%.lhs Main%.lhs Test%.c Main%.c,$(SRCS) $(SRCSC))
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

LIBMODULES = $(patsubst %.o,%,$(LIBOBJSLHS) $(LIBOBJSHS))
SPLITOBJS = $(patsubst %,%/*.o,$(LIBMODULES))

HSFILESALL = $(patsubst %.hs,$$PWD/%.hs,$(SRCS)) \
             $(patsubst %.lhs,$$PWD/%.lhs,$(SRCSLHS)) \
             $(patsubst %.boot.hs,$$PWD/%.boot.hs,$(BOOTSRCS))
OBJSEMACSFULL = $(patsubst %,$$PWD/%,$(OBJSEMACS))
EXPORTSRCSFULL = $(patsubst %,$$PWD/%,$(EXPORTSRCS))
EXPORTHIFILES = $(patsubst %,$$PWD/%,$(HILIBFILES))

EXTRAEXPORTSFULL = $(patsubst %,$$PWD/%,$(EXTRAEXPORTS))

# Can't be bothered to have a special variable for C header files.
# Instead we decree that all C files must have an associated header
# file.
OTHERSALL = $(patsubst %.c,$$PWD/%.c,$(SRCSC))  \
            $(patsubst %.el,$$PWD/%.el,$(SRCSEMACS)) \
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
.PHONY : depend libhere lib testhere test mainhere main all clean cleanprogs ghci ghcihere libfast libfasthere displaysrcshere displayhshere displaysrcs displayhs objsc objschere objsemacs objsemacshere packageherequick packagehere packages packagesquick boot boothere prepareexports prepareexportshere displayexports displayexportshere oldclean exportnames $(EXPORTPREFIX).tar.gz $(EXPORTPREFIX).zip exports www wwwtest wwwhere makefilequick

# The following gmake-3.77ism prevents gmake deleting all the
# object files once it has finished with them, so remakes
# actually work.
.SECONDARY : $(OBJS) $(HIFILES)

# all is the default target anyway, by virtual of boilerplate.mk.
all : packagehere mainhere testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) all && ) echo Finished make all

# ghci starts up GHC interactively, with the current package if any
ghci :
	$(HC) $(TESTFLAGS)  --interactive -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances

# ghcihere starts up GHC and loads the packages this directory depends on,
# but not the package in this directory. 
ghcihere:
	$(HC) $(HCFLAGS)  --interactive -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances

test : testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) test && ) echo Finished make test

main : mainhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) main && ) echo Finished make main

lib : libhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) lib && ) echo Finished make lib

libfast : libfasthere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) libfast && ) echo Finished make libfast

clean: cleanprogs
	$(RM) -rf `$(GFIND) -name "*.hi" -o -name "*.o" -o -name "*.a" -o -name ".depend"`

cleanprogs:
	$(RM) -rf $(TESTPROGS) $(MAINPROGS)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) cleanprogs && ) echo Finished make cleanprogs


oldclean:
	$(RM) -f $(TESTPROGS) $(MAINPROGS) $(OBJS) $(HIBOOTFILES) $(HIFILES) $(LIB)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) clean && ) echo Finished make oldclean

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
	@echo OBJSEMACS = $(OBJSEMACS)
	@echo OBJS = $(OBJS)
	@echo LIBOBJS = $(LIBOBJS)
	@echo TESTOBJS = $(TESTOBJS)
	@echo TESTPROGS = $(TESTPROGS)
	@echo MAINOBJS = $(MAINOBJS)
	@echo MAINPROGS = $(MAINPROGS)
	@echo HIFILES = $(HIFILES)
	@echo HIBOOTFILES = $(HIBOOTFILES)
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
ifneq "$(PACKAGE)" ""
ifeq "$(DOSPLIT)" ""
	$(HC) --make -package-name $(PACKAGE) $(HCFLAGS) $(LIBSRCS)
	$(AR) -r $(LIB) $(LIBOBJS)
else
	for i in $(LIBMODULES) ; do $(MKDIR) -p $$i ; done
	$(HC) --make -package-name $(PACKAGE) $(HCFLAGS) $(LIBSRCS) -split-objs
	$(AR) -r $(LIB) $(SPLITOBJS) $(OBJSC)
endif
endif

packagehere : libfasthere packageherequick

packages : packagehere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packages && ) echo Finished make packages

packageherequick :
# The "echo" after the --remove-package means we keep going even if
# remove-package complains about the package not being there (which it won't
# be, the first time we use this).
ifneq "$(PACKAGE)" ""
	$(GHCPKG) --config-file $(PACKAGECONF) --remove-package $(PACKAGE) ; echo ""
ifneq "$(DOIMPORTS)" ""
# Copy import files over.
	$(RM) -rf imports
	$(MKDIR) imports
	$(CP) $(HILIBFILES) imports
endif
	$(SED) -e 's+PACKAGE+$(PACKAGE)+g;s+IMPORTS+$(if $(DOIMPORTS),/imports)+g;s+DEPS+$(DEPS)+g' <$(TOP)/package.spec.template | $(FIXFILENAMES) | $(GHCPKG) $(GHCPKGOPTS) --config-file $(PACKAGECONF) --add-package 
	if [ -s $(LIB) ]; then $(LD) -r --whole-archive -o $(GHCIOBJ) $(LIB); fi
endif

packagesquick : packageherequick
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packagesquick && ) echo Finished make packagesquick


prepareexportshere : 
ifneq "$(PACKAGE)" ""
	$(GHCPKG) --config-file $(PACKAGECONF).export --remove-package $(PACKAGE) ; echo ""
	PWD=`pwd`;SUFFIX=`expr $$PWD : "$(TOP)/\\\\(.*\\\\)"`;$(SED) -e 's+PACKAGE+$(PACKAGE)+g;s+IMPORTS+$(if $(DOIMPORTS),/imports)+g;s+DEPS+$(DEPS)+g;s+#PWD+#PWD/'$$SUFFIX+g <$(TOP)/package.spec.template | $(GHCPKG) $(GHCPKGOPTS) --config-file $(PACKAGECONF).export --add-package 
endif

prepareexports : prepareexportshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) prepareexports && ) echo Finished make prepareexports

displayexportshere :
ifeq "$(PACKAGE)" ""
	@PWD=`pwd`;echo $(EXPORTSRCSFULL) $(EXTRAEXPORTSFULL)
else
	@PWD=`pwd`;echo $(EXPORTSRCSFULL) $(EXTRAEXPORTSFULL) $$PWD/$(LIB) $$PWD/$(GHCIOBJ) $(OBJSEMACSFULL)
ifeq "$(DOIMPORTS)" ""
	@PWD=`pwd`;echo $(EXPORTHIFILES)
else
	@PWD=`pwd`;echo $$PWD/imports/*.hi
endif
endif

displayexports : displayexportshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) displayexports && ) echo

displaysrcshere :
	@PWD=`pwd`;echo $(ALLFILESALL)

displayhshere :
	@PWD=`pwd`;echo $(LIBSRCS)

displaysrcs : displaysrcshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) displaysrcs && ) echo 

displayhs : displayhshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) displayhs && ) echo

objschere : $(OBJSC)
objsc : objschere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) objsc && ) echo

objsemacshere : $(OBJSEMACS)
objsemacs : objsemacshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) objsemacs && ) echo

doc : dochere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) doc && ) echo 

dochere :

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

boothere : $(HIBOOTFILES)

boot : boothere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) boot &&) echo $(MAKE) boot finished.

$(HIBOOTFILES) : %.hi-boot : %.boot.hs
	$(RM) $@
# The only thing we do here is preprocess the file.
	$(CPP) $< -o $@ -I$(CINCLUDES)
   

$(LIBOBJSHS) : %.o : %.hs
ifeq "$(DOSPLIT)" ""
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS)
else
	$(MKDIR) -p $(@D)/$(*F)
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS) -split-objs
endif


$(LIBOBJSLHS) : %.o : %.lhs
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS) 

TESTFLAGS = $(if $(PACKAGE),$(HCSHORTFLAGS) $(THISPACKAGE),$(HCFLAGS))

$(TESTOBJSHS) $(MAINOBJSHS) : %.o : %.hs
	$(HC) -c $< $(TESTFLAGS)

$(TESTOBJSLHS) $(MAINOBJSLHS) : %.o : %.lhs
	$(HC) -c $< $(TESTFLAGS)

# C objects ought to depend on the header file as well,
# but this is tricky when the C file is inside a subdirectory
# of this one.
$(OBJSC) : %.o : %.c
	$(CCH) $(CFLAGS) -c $< -o $@

$(OBJSEMACS) : %.elc : %.el
	$(GNUCLIENT) -batch -eval '(byte-compile-file "'$$PWD'/$<")'


ifndef FAST
-include .depend
endif
# The dependencies file from the current directory.
# (trick stolen from GHC make files - setting FAST will sneakily
# prevent Make trying to recompile any of the dependent files.)

exportnames :
	($(MAKE) -s --no-print-directory displayexports) | $(SED) "s+^$(TOP)/+uni/+;s+ $(TOP)/+ uni/+g;"

# The name of the export file
EXPORTPREFIX = uni-$(UNIVERSION)-$(OSTITLE)-ghc-$(GhcVersion)

exports : $(EXPORTPREFIX).tar.gz $(EXPORTPREFIX).zip

$(EXPORTPREFIX).tar.gz :
	DIR=`pwd`;cd $(TOP)/..;$(TAR) -czhf $$DIR/$@ `$(MAKE) -s --no-print-directory -C $$DIR exportnames`

$(EXPORTPREFIX).zip :
	DIR=`pwd`;cd $(TOP)/..;$(ZIP) -9 $$DIR/$@ `$(MAKE) -s --no-print-directory -C $$DIR exportnames`


# www stuff
www :
	$(if $(WWWPREFIX),$(MAKE) wwwtest;chmod -R a+rX $(WWWPREFIX),echo WWWPREFIX must be set to make www)

wwwtest : wwwhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) wwwtest && ) echo 

# rename Makefile in current directory
makefilequick : 
	$(TOP)/config.status --file=Makefile



