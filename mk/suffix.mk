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
#    $(LIB).
# The variables the Makefiles in the individual subdirectories
# need to set, if non-null, are
# SUBDIRS  the list of subdirectories to recurse to, which should
#             contain another Makefile and be in an order in which they
#             can be compiled.
# SRCS     all Haskell source files
# SRCSC    all C source files
# LIB      the name of the desired library
# LIBS     libraries required by the test programs (if any).
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

OBJSHS = $(patsubst %.hs,%.o,$(SRCS))
OBJSC = $(patsubst %.c,%.o,$(SRCSC))
OBJS = $(OBJSHS) $(OBJSC)
LIBOBJS = $(filter-out Test%.o Main%.o,$(OBJS))
TESTOBJS = $(filter Test%.o,$(OBJS))
TESTPROGS = $(patsubst Test%.o,test%,$(TESTOBJS))
MAINOBJS = $(filter Main%.o,$(OBJS))
MAINPROGS = $(patsubst Main%.o,%,$(MAINOBJS))
HIFILES = $(patsubst %.hs,%.hi,$(SRCS))
#
#
# Here are some phony targets
#

# Specify that these targets don't correspond to files.
.PHONY : depend libhere lib testhere test all clean display

# The following gmake-3.77ism prevents gmake deleting all the
# object files once it has finished with them, so remakes
# actually work.
.SECONDARY : $(OBJS) $(HIFILES)

# all is the default target anyway, by virtual of boilerplate.mk.
all : testhere libhere mainhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) all && ) echo Finished make all

test : testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) test && ) echo Finished make test

main : mainhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) main && ) echo Finished make main

lib : libhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) lib && ) echo Finished make lib

clean:
	$(RM) -f $(TESTPROGS) $(MAINPROGS) $(OBJS) $(LIB) $(patsubst %.o,%.hi,$(OBJS))
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) clean && ) echo Finished make clean

display :
	@echo SRCS = $(SRCS)
	@echo SRCSC = $(SRCSC)
	@echo LIB = $(LIB)
	@echo LIBS = $(LIBS)
	@echo OBJSHS = $(OBJSHS)
	@echo OBJSC = $(OBJSC)
	@echo OBJS = $(OBJS)
	@echo LIBOBJS = $(LIBOBJS)
	@echo TESTOBJS = $(TESTOBJS)
	@echo TESTPROGS = $(TESTPROGS)
	@echo MAINOBJS = $(MAINOBJS)
	@echo MAINPROGS = $(MAINPROGS)
	@echo HIFILES = $(HIFILES)

depend : $(SRCS) 
	$(DEPEND) $(HCSYSLIBS) -i$(HCDIRS) $(SRCS)
	$(foreach subdir,$(SUBDIRS),$(MAKE) -r -C $(subdir) depend && ) echo Finished make depend

ifeq ($(LIBOBJS),)
# no library
   libhere :
else
   libhere : $(LIB)
endif

testhere : $(TESTPROGS)

mainhere : $(MAINPROGS)

$(LIB) : $(LIBOBJS)
	$(RM) $@ ; $(AR) -r $@ $^

$(TESTPROGS) : test% :  Test%.o $(LIBS) $(LIB)
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LIB) $(LIBS)

$(MAINPROGS) : % :  Main%.o $(LIBS) $(LIB)
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LIB) $(LIBS)

$(HIFILES) : %.hi : %.o
	@:

$(OBJSHS) : %.o : %.hs
	$(HC) -c $< $(HCFLAGS) 

$(OBJSC) : %.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

ifndef FAST
-include .depend
endif
# The dependencies file from the current directory.
# (trick stolen from GHC make files - setting FAST will sneakily
# prevent Make trying to recompile any of the dependent files.)




