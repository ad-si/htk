HCINC      = 

LIBS       = ../libtools.a\
	     $(HTKDIR)/libhtk.a\
	     $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a\
	     $(REGEXPLIBDIR)/libRegexp.a\

LIB        = 

SRCS       = 
      
SRCSH      = 

SRCSC      = 

OLDOBJS       = 

PGMS       = testdiff\
		testgrep\
		testholcsp\
		testholz\
		testhugs1\
		testhugs2\
		testlatex\
		testmoby\
		testtextedit\


PGM_OBJS   = TestDiff.o\
	 TestGrep.o\
	 TestHolCSP.o\
	 TestHolZ.o\
	 TestHugs1.o\
	 TestHugs2.o\
	 TestLatex.o\
	 TestMoby.o\
	 TestTextEdit.o\


EXTRA_DEPS =  TestDiff.hs\
	 TestGrep.hs\
	 TestHolCSP.hs\
	 TestHolZ.hs\
	 TestHugs1.hs\
	 TestHugs2.hs\
	 TestLatex.hs\
	 TestMoby.hs\
	 TestTextEdit.hs\


EXTRA_HC_OPTIONS += -H30M


all:
	$(MAKE) testdiff
	$(MAKE) testgrep
	$(MAKE) testholcsp
	$(MAKE) testholz
	$(MAKE) testhugs1
	$(MAKE) testhugs2
	$(MAKE) testlatex
	$(MAKE) testmoby
	$(MAKE) testtextedit


testdiff:    $(LIBS) $(LIB) TestDiff.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestDiff.o $(LIB) $(LIBS)


testgrep:    $(LIBS) $(LIB) TestGrep.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestGrep.o $(LIB) $(LIBS)


testholcsp:    $(LIBS) $(LIB) TestHolCSP.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestHolCSP.o $(LIB) $(LIBS)


testholz:    $(LIBS) $(LIB) TestHolZ.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestHolZ.o $(LIB) $(LIBS)


testhugs1:    $(LIBS) $(LIB) TestHugs1.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestHugs1.o $(LIB) $(LIBS)


testhugs2:    $(LIBS) $(LIB) TestHugs2.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestHugs2.o $(LIB) $(LIBS)

testlatex:    $(LIBS) $(LIB) TestLatex.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestLatex.o $(LIB) $(LIBS)

testmoby:    $(LIBS) $(LIB) TestMoby.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestMoby.o $(LIB) $(LIBS)

testtextedit:    $(LIBS) $(LIB) TestTextEdit.o  
	$(RM) $@
	$(HC) -o $@ $(HCFLAGS) TestTextEdit.o $(LIB) $(LIBS)


