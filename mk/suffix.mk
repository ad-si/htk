# We establish the following conventions.
# Haskell files end with ".hs", c with ".c".
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
OBJSC = $(patsubst %.c,%.o,$(SRCSC))
OBJSEMACS = $(patsubst %.el,%.elc,$(SRCSEMACS))
OBJS = $(OBJSHS)  $(OBJSC)
LIBSRCS = $(filter-out Test%.hs Main%.hs,$(SRCS))
HADDOCKSRCS = $(patsubst %,haddock/%,$(LIBSRCS))
EXPORTSRCS = $(filter Test%.hs Main%.hs Test%.c Main%.c,$(SRCS) $(SRCSC)) $(SRCSEMACS)
LIBOBJS = $(filter-out Test%.o Main%.o,$(OBJS))
TESTOBJS = $(filter Test%.o,$(OBJS))
TESTPROGS = $(patsubst Test%.o,test%,$(TESTOBJS))
MAINOBJS = $(filter Main%.o,$(OBJS))
MAINPROGS = $(patsubst Main%.o,%,$(MAINOBJS))

LIBOBJSHS = $(filter-out Test%.o Main%.o,$(OBJSHS))

# Construct list of module names for the benefit of ghc-pkg
LIBMODULENAMES1 = $(LIBOBJSHS:.o=)
LIBMODULENAMES = $(subst /,.,$(LIBMODULENAMES1))

TESTOBJSHS = $(filter Test%.o,$(OBJSHS))
MAINOBJSHS = $(filter Main%.o,$(OBJSHS))

HILIBFILES = $(patsubst %.o,%.hi,$(LIBOBJSHS))
HIFILES = $(patsubst %.o,%.hi,$(OBJSHS))
HIBOOTFILES = $(patsubst %.hs-boot,%.hi-boot,$(BOOTSRCS))

LIBMODULES = $(patsubst %.o,%,$(LIBOBJSHS))
SPLITOBJS = $(patsubst %,%/*.o,$(LIBMODULES))

HSFILESALL = $(patsubst %.hs,$$PWD/%.hs,$(SRCS)) \
             $(patsubst %.hs-boot,$$PWD/%.hs-boot,$(BOOTSRCS))
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
DEPS' = $(filter-out BEGINCOMMA BEGIN,BEGIN$(PACKAGES:%=COMMA %))
DEPS = $(DEPS':COMMA=,)

LIBMODULENAMESCOMMAS' = $(filter-out BEGINCOMMA BEGIN,BEGIN$(LIBMODULENAMES:%=COMMA %))

LIBMODULENAMESCOMMAS = $(LIBMODULENAMESCOMMAS':COMMA=,)
#
#
# Here are some phony targets
#

# Specify that these targets don't correspond to files.
.PHONY : dependhere depend libhere lib testhere test mainhere main all clean cleanprogs ghci ghcihere libfast libfasthere displaysrcshere displayhshere displaysrcs displayhs objsc objschere objsemacs objsemacshere packageherequick packagehere packages packagesquick boot boothere prepareexports prepareexportshere displayexports displayexportshere oldclean exportnames $(EXPORTPREFIX).tar.gz $(EXPORTPREFIX).zip exports www wwwtest wwwhere makefilequick preparehaddock preparehaddockhere haddock haddockhere copyhaddocksources haddockgenindex slow slowhere cabal cabalhere

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

slow : slowhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) slow && ) echo Finished make slow

slowhere : dependhere
	$(MAKE) -r libhere packageherequick

clean: cleanprogs
	$(RM) -rf `$(GFIND) . \( \! -path "./HaXml-*" \) \( -name "*.hi" -o -name "*.hi-boot" -o -name "*.o" -o -name "*.a" -o -name ".depend" \)`

cleanprogs:
	$(RM) -rf $(TESTPROGS) $(MAINPROGS)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) cleanprogs && ) echo Finished make cleanprogs


oldclean:
	$(RM) -f $(TESTPROGS) $(MAINPROGS) $(OBJS) $(HIBOOTFILES) $(HIFILES) $(LIB)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) clean && ) echo Finished make oldclean

display :
	@echo SRCS = $(SRCS)
	@echo BOOTSRCS = $(BOOTSRCS)
	@echo SRCSC = $(SRCSC)
	@echo LIB = $(LIB)
	@echo LIBS = $(LIBS)
	@echo OBJSHS = $(OBJSHS)
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
	@echo LIBMODULENAMES = $(LIBMODULENAMES)
	@echo LIBMODULENAMESCOMMAS = $(LIBMODULENAMESCOMMAS)

depend : dependhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) depend && ) echo Finished make depend

dependhere : $(SRCS) $(BOOTSRCS)

ifneq "$(strip $(SRCS))" ""
	$(DEPEND) $(HCFLAGS) $(SRCS)
endif

ifeq "$(strip $(LIBOBJS))" ""
# no library
   libhere :
else
   libhere : $(LIB)
endif

testhere : $(TESTPROGS)

mainhere : $(MAINPROGS)

libfasthere : $(OBJSC)
ifneq "$(PACKAGE)" ""
ifneq "$(strip $(LIBSRCS))" ""
	$(HC) --make -package-name $(PACKAGE) $(HCFLAGS) $(LIBSRCS)
endif
ifneq "$(strip $(LIBOBJS))" ""
	$(AR) -rs $(LIB) $(LIBOBJS)
endif
endif


SETUP = $(GHCTOP)/Setup

$(SETUP) : $(SETUP).hs
	$(HC) --make -o $@ $<

CABALGHCPKG = --with-hc-pkg=$(TOP)/mk/cabal-ghc-pkg

CABAL = $(SETUP) configure -O $(CABALGHCPKG) \
        -w $(GHCTOP)/mk/cabal-ghc --prefix=$(TOP) --user; \
    $(SETUP) build; \
    $(SETUP) copy; \
    $(SETUP) register --gen-script; \
    ./register.sh

CABALFILE = $(wildcard *.cabal)
cabalhere : $(SETUP)
	if [ -n "$(CABALFILE)" ]; then $(CABAL) ; fi

cabal : cabalhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) cabal && ) echo Finished make cabal

packagehere : libfasthere packageherequick

packages : packagehere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packages && ) echo Finished make packages

# Option to be given to ld for producing GHCi object files.
WHOLEARCHIVE = $(if $(findstring MacOS,$(OSTITLE)),-all_load,--whole-archive)

PACKAGELIB=$(if $(LIBOBJS),$(PACKAGE),)

packageherequick :
# The "echo" after the --remove-package means we keep going even if
# remove-package complains about the package not being there (which it won't
# be, the first time we use this).
ifneq "$(PACKAGE)" ""
	if [ -f $(PACKAGE).o -a -f lib$(PACKAGE).a ]; then \
	   $(GFIND) lib$(PACKAGE).a -maxdepth 0 -newer $(PACKAGE).o -exec \
	      $(RM) $(PACKAGE).o \; ; fi
	$(GHCPKG) --config-file $(PACKAGECONF) --remove-package $(PACKAGE) ; echo ""
	$(SED) -e 's+PACKAGELIB+$(PACKAGELIB)+g;s+PACKAGE+$(PACKAGE)+g;s+IMPORTS++g;s+DEPS+$(DEPS)+g;s+EXPOSED+$(LIBMODULENAMESCOMMAS)+g;' <$(TOP)/package.spec.template | $(FIXFILENAMES) | $(GHCPKG) $(GHCPKGOPTS) --config-file $(PACKAGECONF) --force --add-package --auto-ghci-libs
endif

packagesquick : packageherequick
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) packagesquick && ) echo Finished make packagesquick


prepareexportshere :
ifneq "$(PACKAGE)" ""
	$(GHCPKG) --config-file $(PACKAGECONF).export --remove-package $(PACKAGE) ; echo ""
	PWD=`pwd`;SUFFIX=`expr $$PWD : "$(TOP)/\\\\(.*\\\\)"`;$(SED) -e 's+PACKAGELIB+$(PACKAGELIB)+g;s+PACKAGE+$(PACKAGE)+g;s+IMPORTS++g;s+DEPS+$(DEPS)+g;s+EXPOSED+$(LIBMODULENAMESCOMMAS)+g;s+#PWD+#PWD/'$$SUFFIX+g <$(TOP)/package.spec.template | $(GHCPKG) $(GHCPKGOPTS) --config-file $(PACKAGECONF).export --force --add-package
endif

prepareexports : prepareexportshere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) prepareexports && ) echo Finished make prepareexports

displayexportshere :
ifneq "$(strip $(LIBOBJS))" ""
	@PWD=`pwd`;echo $(EXPORTSRCSFULL) $(EXTRAEXPORTSFULL) $$PWD/$(LIB) $$PWD/$(GHCIOBJ) $(OBJSEMACSFULL)
	@PWD=`pwd`;echo $(EXPORTHIFILES)
else
	@PWD=`pwd`;echo $(EXPORTSRCSFULL) $(EXTRAEXPORTSFULL)
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
	$(RM) $@ ; $(AR) -rs $@ $^

$(TESTPROGS) : test% :  Test%.o
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LINKFLAGS) $(THISPACKAGE)

$(MAINPROGS) : % :  Main%.o
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LINKFLAGS) $(THISPACKAGE)

$(HIFILES) : %.hi : %.o
	@:

$(LIBOBJSHS) : %.o : %.hs
ifeq "$(DOSPLIT)" ""
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS)
else
	$(MKDIR) -p $(@D)/$(*F)
	$(HC) -c -package-name $(PACKAGE) $< $(HCFLAGS) -split-objs
endif


TESTFLAGS = $(if $(PACKAGE),$(HCSHORTFLAGS) $(THISPACKAGE),$(HCFLAGS))

$(TESTOBJSHS) $(MAINOBJSHS) : %.o : %.hs
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


# running Haddock
haddock : haddockhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) haddock && ) echo
	$(MAKE) haddockgenindex

haddockhere : preparehaddockhere copyhaddocksources
ifneq "$(strip $(LIBOBJS))" ""
# Here follows some revolting shell hackery to set up the following
# variables.  Supposing uni is installed in /home/spqr/uni;
# this Makefile is being run in /home/spqr/uni/foo/bar and
# there is a interface.haddock file in /home/spqr/uni/baz,
# then
# PWD=/home/spqr/uni/foo/bar
# SUFFIX=foo/bar
# TOPRELATIVE=../.. (so location of top directory relative to this one)

	PWD=`pwd`;SUFFIX=`expr $$PWD : "$(TOP)/\\\\(.*\\\\)"`; \
	TOPRELATIVE=`echo $$SUFFIX | sed -e 's/[^/][^/]*/../g'`; \
	rm -f $(TOP)/www/$$SUFFIX/interface.haddock; \
	HINTERFACES0=`$(GFIND) $(TOP)/www -name '*.haddock' \
           -printf "--read-interface=$$TOPRELATIVE/%P,%p "` \
	HINTERFACES=`echo $$HINTERFACES0 | sed -e 's+/[^/]*.haddock,+,+g'` ;\
        mkdir -p $(TOP)/www/$$SUFFIX ; \
	cd haddock; \
	PROLOGUE=`if [ -r ../haddock.prologue ]; then echo -p ../haddock.prologue; fi`; \
	$(HADDOCK) -v -h --package $(PACKAGE) -o $(TOP)/www/$$SUFFIX \
           -s sources/%F -D $(TOP)/www/$$SUFFIX/interface.haddock \
	   $$PROLOGUE $$HINTERFACES $(LIBSRCS)
endif

haddockgenindex:
ifdef THISISTOP
	HADDOCKINTERFACES0=`$(GFIND) $(TOP)/www -name interface.haddock \
           -printf "--read-interface=%P,%p "`; \
	HADDOCKINTERFACES=`echo $$HADDOCKINTERFACES0 | sed -e s+/interface.haddock,+,+g` ;\
	$(HADDOCK) -v -o $(TOP)/www \
	   $$HADDOCKINTERFACES \
          --gen-index --gen-contents
endif

copyhaddocksources :
ifneq "$(strip $(LIBOBJS))" ""
	PWD=`pwd`;SUFFIX=`expr $$PWD : "$(TOP)/\\\\(.*\\\\)"`; \
           $(foreach src,$(LIBSRCS), \
	      mkdir -p `dirname $(TOP)/www/$$SUFFIX/sources/$(src)` && \
	      cp -f $(src) $(TOP)/www/$$SUFFIX/sources/$(src) &&) echo
endif


# Preparing file for Haddock
preparehaddock : preparehaddockhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) preparehaddock && ) echo

preparehaddockhere : $(HADDOCKSRCS)

mkHaddock = $(TOP)/mk/RemoveSplices <$(1) | grep -v '{-\# SOURCE \#-}' >$(2)

$(HADDOCKSRCS) : haddock/%.hs : %.hs
	$(MKDIR) -p `dirname $@`
ifdef DOCPP
	$(CPP) $< -optP-P -o /tmp/cpp.out
	$(call mkHaddock,/tmp/cpp.out,$@)
else
	$(call mkHaddock,$<,$@)
endif

