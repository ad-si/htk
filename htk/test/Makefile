HCINC      = 

LIBS       = ../libhtk.a\
	     $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a

LIB        = 

SRCS       = 
      
SRCSH      = 

SRCSC      = 

OLDOBJS       = 

PGMS       = test1 test2 test3 test9 test10

PGM_OBJS   = Counter.o\
	 TestUtil.o\
	 Test1.o\
	 Test2.o\
	 Test3.o\
 	 Test9.o\
         Test10.o\


EXTRA_DEPS =  Counter.hs\
	 TestUtil.hs\
	 Test1.hs\
	 Test2.hs\
	 Test3.hs\
 	 Test9.hs\
         Test10.hs


EXTRA_HC_OPTIONS += -H30M


all: $(PGMS)

include top/mk/boilerplate.mk

test0:    $(LIBS) $(LIB) Counter.o TestUtil.o Test0.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Counter.o TestUtil.o Test0.o $(LIB) $(LIBS)

test1:    $(LIBS) $(LIB) Counter.o TestUtil.o Test1.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Counter.o TestUtil.o Test1.o $(LIB) $(LIBS)

test2:    $(LIBS) $(LIB) Counter.o TestUtil.o Test2.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Counter.o TestUtil.o  Test2.o $(LIB) $(LIBS)

test3:    $(LIBS) $(LIB) Counter.o TestUtil.o Test3.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Counter.o TestUtil.o Test3.o $(LIB) $(LIBS)

test9:    $(LIBS) $(LIB) TestUtil.o Counter.o Test9.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestUtil.o Counter.o  Test9.o $(LIB) $(LIBS)

test10:    $(LIBS) $(LIB) TestUtil.o Counter.o Test10.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestUtil.o Counter.o  Test10.o $(LIB) $(LIBS)

bug3:      $(LIBS) $(LIB) Bug3.o
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Bug3.o $(LIB) $(LIBS)
# DO NOT DELETE: Beginning of Haskell dependencies
# DO NOT DELETE: End of Haskell dependencies
