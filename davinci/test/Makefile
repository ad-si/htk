HCINC      = 

LIBS       = ../libdavinci.a\
	     $(HTKDIR)/libhtk.a\
	     $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a

LIB        = 

SRCS       = 
      
SRCSH      = 

SRCSC      = 

OLDOBJS       = 

PGMS       = pgm test1 test2 test3

PGM_OBJS   =  Main.o\
		Test1.o\
		Test2.o\
		Test3.o\

 
EXTRA_DEPS =  Main.hs\
		Test1.hs\
		Test2.hs\
		Test3.hs\


EXTRA_HC_OPTIONS += -O -H30M

include top/mk/boilerplate.mk

all:
#	$(MAKE) pgm
	$(MAKE) test1
	$(MAKE) test2
	$(MAKE) test3

pgm:    $(LIBS) $(LIB) Main.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Main.o $(LIB) $(LIBS)

test1:    $(LIBS) $(LIB) Test1.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test1.o $(LIB) $(LIBS)

test2:    $(LIBS) $(LIB) Test2.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test2.o $(LIB) $(LIBS)

test3:    $(LIBS) $(LIB) Test3.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Test3.o $(LIB) $(LIBS)


