HCINC      = 

LIBS       = ../libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a\
#	     $(REGEXPLIBDIR)/libRegexp.a\
	     

LIB        = 

SRCS       = 

SRCSH      = 

SRCSC      = 

OLDOBJS       =            


PGMS       = test0\
             test1\
	     test2\
	     test3\
	     test4\
	     test5\
	     test6\
	     test7\
	     test8\
	     test9\
	     test10\


PGM_OBJS   = Test0.o\
             Test1.o\
	     Test2.o\
	     Test3.o\
	     Test4.o\
	     Test5.o\
	     Test6.o\
	     Test7.o\
	     Test8.o\
	     Test9.o\
	     Test10.o\

EXTRA_DEPS = Test0.hs\
             Test1.hs\
	     Test2.hs\
	     Test3.hs\
	     Test4.hs\
	     Test5.hs\
	     Test6.hs\
	     Test7.hs\
	     Test8.hs\
	     Test9.hs\
	     Test10.hs\


EXTRA_HC_OPTIONS += -H30M -fno-prune-tydecls

include top/mk/boilerplate.mk

all:	$(LIBS) $(LIB) $(PGM_OBJS)
	$(MAKE) test0
	$(MAKE) test1
	$(MAKE) test2
	$(MAKE) test3
	$(MAKE) test4
	$(MAKE) test5
	$(MAKE) test6
	$(MAKE) test7
	$(MAKE) test8
	$(MAKE) test9
	$(MAKE) test10


test0:  $(LIBS) $(LIB) Test0.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test0.o $(LIB) $(LIBS)

test1:  $(LIBS) $(LIB) Test1.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test1.o $(LIB) $(LIBS)

test2:  $(LIBS) $(LIB) Test2.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test2.o $(LIB) $(LIBS)


test3:  $(LIBS) $(LIB) Test3.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test3.o $(LIB) $(LIBS)

test4:  $(LIBS) $(LIB) Test4.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test4.o $(LIB) $(LIBS)

test5:  $(LIBS) $(LIB) Test5.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test5.o $(LIB) $(LIBS)

test6:  $(LIBS) $(LIB) Test6.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test6.o $(LIB) $(LIBS)

test7:  $(LIBS) $(LIB) Test7.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test7.o $(LIB) $(LIBS)

test8:  $(LIBS) $(LIB) Test8.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test8.o $(LIB) $(LIBS)

test9:  $(LIBS) $(LIB) Test9.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test9.o $(LIB) $(LIBS)

test10:  $(LIBS) $(LIB) Test10.o 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test10.o $(LIB) $(LIBS)


