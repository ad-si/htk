HCINC      = 

LIBS       = $(HTKDIR)/libhtk.a\
             $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a

LIB        = 

SRCS       = 
      
SRCSH      = 

SRCSC      = 

PGMS       = hello1 hello2 hello3 \
	     canvas_ex xmas_canvas blobs textwid_ex \
	     relief_ex list_ex menu_ex

PGM_OBJS   = Hello1.o Hello2.o Hello3.o \
	     ReliefEx.o ListEx.o MenuEx.o \
	     CanvasEx.o XmasCanvas.o Blobs.o TextWidEx.o

EXTRA_DEPS = Hello1.hs Hello2.hs Hello3.hs\
	     ReliefEx.hs ListEx.hs MenuEx.hs \
	     CanvasEx.hs XmasCanvas.hs Blobs.hs TextWidEx.hs

EXTRA_HC_OPTIONS += -H30M

all: $(PGMS)

include top/mk/boilerplate.mk

hello1:  $(LIBS) $(LIB) Hello1.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Hello1.o $(LIB) $(LIBS)

hello2:  $(LIBS) $(LIB) Hello2.o
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Hello2.o $(LIB) $(LIBS)

hello3:  $(LIBS) $(LIB) Hello3.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Hello3.o $(LIB) $(LIBS)

relief_ex: $(LIBS) $(LIB) ReliefEx.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) ReliefEx.o $(LIB) $(LIBS)

list_ex: $(LIBS) $(LIB) ListEx.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) ListEx.o $(LIB) $(LIBS)

menu_ex: $(LIBS) $(LIB) MenuEx.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) MenuEx.o $(LIB) $(LIBS)

canvas_ex: $(LIBS) $(LIB) CanvasEx.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) CanvasEx.o $(LIB) $(LIBS)

xmas_canvas: $(LIBS) $(LIB) XmasCanvas.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) XmasCanvas.o $(LIB) $(LIBS)

blobs: $(LIBS) $(LIB) Blobs.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) Blobs.o $(LIB) $(LIBS)

textwid_ex: $(LIBS) $(LIB) TextWidEx.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TextWidEx.o $(LIB) $(LIBS)

clean:
	$(RM) $(PGMS)
	$(RM) *.o 
	$(RM) Main.hi

# DO NOT DELETE: Beginning of Haskell dependencies
# DO NOT DELETE: End of Haskell dependencies
