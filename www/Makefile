THIS 	   = www

IMPS	   = *.hi

HCINC      = 

LIBS       = $(HTKDIR)/libhtk.a\
	     $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a\

LIB        = libwww.a

SRCS       = HtmlTypes.hs\
	 HtmlClasses.hs\
	 HtmlKernel.hs\
	 HtmlList.hs\
	 HtmlBlock.hs\
	 HtmlText.hs\
	 HtmlLink.hs\
	 HtmlMeta.hs\
	 HtmlForm.hs\
	 HtmlApplet.hs\
	 HtmlTable.hs\
	 HtmlFrame.hs\
	 Html.hs\
	 TableGen.hs
 
      
SRCSH      = 

SRCSC      = 

OBJS       = HtmlTypes.o\
	 HtmlClasses.o\
	 HtmlKernel.o\
	 HtmlList.o\
	 HtmlBlock.o\
	 HtmlText.o\
	 HtmlLink.o\
	 HtmlMeta.o\
	 HtmlForm.o\
	 HtmlApplet.o\
	 HtmlTable.o\
	 HtmlFrame.o\
	 Html.o\
	 TableGen.o            

PGMS       = 

PGM_OBJS   =  

EXTRA_DEPS =  

EXTRA_HC_OPTIONS += -H30M

include top/mk/boilerplate.mk

# DO NOT DELETE: Beginning of Haskell dependencies
HtmlTypes.o : HtmlTypes.hs
HtmlTypes.o : top/util/Debug.hi
HtmlClasses.o : HtmlClasses.hs
HtmlClasses.o : top/htk/toplevel/HTk.hi
HtmlClasses.o : ./HtmlTypes.hi
HtmlClasses.o : top/util/Debug.hi
HtmlKernel.o : HtmlKernel.hs
HtmlKernel.o : top/util/Computation.hi
HtmlKernel.o : top/htk/toplevel/HTk.hi
HtmlKernel.o : ./HtmlTypes.hi
HtmlKernel.o : ./HtmlClasses.hi
HtmlKernel.o : top/util/Dynamics.hi
HtmlKernel.o : top/concurrency/Thread.hi
HtmlKernel.o : top/util/Debug.hi
HtmlList.o : HtmlList.hs
HtmlList.o : top/htk/toplevel/HTk.hi
HtmlList.o : ./HtmlKernel.hi
HtmlList.o : top/util/Debug.hi
HtmlBlock.o : HtmlBlock.hs
HtmlBlock.o : top/htk/toplevel/HTk.hi
HtmlBlock.o : ./HtmlKernel.hi
HtmlBlock.o : top/util/Debug.hi
HtmlText.o : HtmlText.hs
HtmlText.o : top/htk/toplevel/HTk.hi
HtmlText.o : ./HtmlKernel.hi
HtmlText.o : top/util/Debug.hi
HtmlLink.o : HtmlLink.hs
HtmlLink.o : top/htk/toplevel/HTk.hi
HtmlLink.o : ./HtmlKernel.hi
HtmlLink.o : ./HtmlTypes.hi
HtmlLink.o : top/util/Debug.hi
HtmlMeta.o : HtmlMeta.hs
HtmlMeta.o : top/htk/toplevel/HTk.hi
HtmlMeta.o : ./HtmlKernel.hi
HtmlMeta.o : ./HtmlLink.hi
HtmlMeta.o : top/util/Debug.hi
HtmlForm.o : HtmlForm.hs
HtmlForm.o : top/htk/toplevel/HTk.hi
HtmlForm.o : ./HtmlKernel.hi
HtmlForm.o : ./HtmlLink.hi
HtmlForm.o : top/htk/resources/Font.hi
HtmlForm.o : top/util/Debug.hi
HtmlApplet.o : HtmlApplet.hs
HtmlApplet.o : top/htk/toplevel/HTk.hi
HtmlApplet.o : ./HtmlKernel.hi
HtmlApplet.o : ./HtmlLink.hi
HtmlApplet.o : top/util/Debug.hi
HtmlTable.o : HtmlTable.hs
HtmlTable.o : top/htk/toplevel/HTk.hi
HtmlTable.o : ./HtmlKernel.hi
HtmlTable.o : ./HtmlLink.hi
HtmlTable.o : ./HtmlText.hi
HtmlTable.o : top/util/Debug.hi
HtmlFrame.o : HtmlFrame.hs
HtmlFrame.o : top/htk/toplevel/HTk.hi
HtmlFrame.o : ./HtmlKernel.hi
HtmlFrame.o : ./HtmlLink.hi
HtmlFrame.o : top/util/Debug.hi
Html.o : Html.hs
Html.o : top/htk/toplevel/HTk.hi
Html.o : top/htk/resources/Colour.hi
Html.o : ./HtmlKernel.hi
Html.o : ./HtmlClasses.hi
Html.o : ./HtmlTypes.hi
Html.o : ./HtmlList.hi
Html.o : ./HtmlBlock.hi
Html.o : ./HtmlText.hi
Html.o : ./HtmlLink.hi
Html.o : ./HtmlMeta.hi
Html.o : ./HtmlForm.hi
Html.o : ./HtmlApplet.hi
Html.o : ./HtmlTable.hi
Html.o : ./HtmlFrame.hi
Html.o : top/util/Debug.hi
TableGen.o : TableGen.hs
TableGen.o : top/util/Computation.hi
TableGen.o : ./Html.hi
TableGen.o : top/util/Date.hi
TableGen.o : top/util/Debug.hi
# DO NOT DELETE: End of Haskell dependencies
