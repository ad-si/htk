HCINC      = 

LIBS       = ../libconcurrency.a $(UTILDIR)/libutil.a 

LIB        = 

SRCS       = 

SRCSH      = 

SRCSC      = 

PGMS       = pgm\
             test0\
             test1\
	     test2\
#	     test3\
	     test4\
	     test5\
	     test6\
	     test7\
	     test8\
	     test9\


PGM_OBJS   = Main.o\
             Test0.o\
	     Test1.o\
	     Test2.o\
#	     Test3.o\
	     Test4.o\
	     Test5.o\
	     Test6.o\
	     Test7.o\
	     Test8.o\
	     Test9.o\

EXTRA_DEPS = Main.hs\
             Test0.hs\
	     Test1.hs\
	     Test2.hs\
	     Test3.hs\
	     Test4.hs\
	     Test5.hs\
	     Test6.hs\
	     Test7.hs\
	     Test8.hs\
	     Test9.hs



include top/mk/boilerplate.mk

EXTRA_HC_OPTIONS += -H30M -Onot

all:	$(LIBS) $(LIB) $(PGM_OBJS)
#	$(MAKE) pgm
	$(MAKE) test0
	$(MAKE) test1
	$(MAKE) test2
#	$(MAKE) test3 -- seems to be about the interactor not concurrency
	$(MAKE) test4
	$(MAKE) test5
	$(MAKE) test6
	$(MAKE) test7
	$(MAKE) test8
	$(MAKE) test9


# pgm:    $(LIBS) $(LIB) Main.o 
#	$(RM) $@
#	$(HC) -o $@ $(HCFLAGS) Main.o $(LIB) $(LIBS)

test%:  Test%.o $(LIBS) 
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $< $(LIBS)




