# We establish the following conventions.
# Haskell files end with ".hs", c with ".c".
# Source files are divided into those which go into the library
# and those which form a test case.  They are distinguished as
# follows: the second class should have names beginning with "Test".
# The purpose of compilation is to put the former class into the
# library $(LIB).  The latter are compiled to an executable with the
# same name except that the suffix has been stripped and the initial
# "Test" has been replaced by "test".
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
LIBOBJS = $(filter-out Test%.o,$(OBJS))
TESTOBJS = $(filter Test%.o,$(OBJS))
TESTPROGS = $(patsubst Test%.o,test%,$(TESTOBJS))
HIFILES = $(patsubst %.hs,%.hi,$(SRCS))
#
#
# Here are some phony targets
#
# Default rule is all
.DEFAULT : all

# Specify that these targets don't correspond to files.
.PHONY : depend libhere lib testhere test all clean display

# The following gmake-3.77ism prevents gmake deleting all the
# object files once it has finished with them, so remakes
# actually work.
.SECONDARY : $(OBJS) $(HIFILES)

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
	@echo HIFILES = $(HIFILES)

depend : $(SRCS) 
	$(DEPEND) $(HCSYSLIBS) -i$(HCDIRS) $(SRCS)

ifeq ($(LIBOBJS),)
# no library
   libhere :
else
   libhere : $(LIB)
endif

testhere : $(TESTPROGS)

test : testhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) test && ) echo Finished make test

lib : libhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) lib && ) echo Finished make lib

all : testhere libhere
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) all && ) echo Finished make all

clean:
	$(RM) -f $(TESTPROGS) $(OBJS) $(LIB) $(patsubst %.o,%.hi,$(OBJS))
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) clean && ) echo Finished make clean

$(LIB) : $(LIBOBJS)
	$(RM) $@ ; $(AR) -r $@ $^

$(TESTPROGS) : test% :  Test%.o $(LIBS) $(LIB)
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LIBS)

$(HIFILES) : %.hi : %.o
	@:

$(OBJSHS) : %.o : %.hs
	$(HC) -c $< $(HCFLAGS) 

$(OBJSC) : %.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@





