HCINC      = 

LIBS       = ../libwww.a\
	     $(HTKDIR)/libhtk.a\
	     $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a

LIB        = 

SRCS       = 
      
SRCSH      = 

SRCSC      = 

OLDOBJS       = 

PGMS       = pgm

PGM_OBJS   =  Main.o
 
EXTRA_DEPS =  Main.hs

include top/mk/boilerplate.mk

EXTRA_HC_OPTIONS += -Onot -H30M

pgm:    $(LIBS) $(LIB) $(PGM_OBJS)  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) $(PGM_OBJS) $(LIB) $(LIBS)


