LIB        = libcvs.a
LIBS = $(UNIDIR)/cvs/libcvs.a $(UNIDIR)/reactor/libreactor.a \
   $(UNIDIR)/server/libserver.a $(UNIDIR)/concurrency/libconcurrency.a \
   $(UNIDIR)/util/libutil.a

SRCS       = CVSTypes.hs CVSBasic.hs CVSHigh.hs CVSDB.hs Allocate.hs \
   AllocateService.hs MainAllocateService.hs

SRCSC      = copy_file.c
SUBDIRS    = test

include top/mk/boilerplate.mk

CVSDB.o : copy_file.h
copy_file.o : copy_file.h