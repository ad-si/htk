LIB        = libcvs.a

SRCS       = CVSTypes.hs CVSBasic.hs CVSHigh.hs CVSDB.hs Allocate.hs
SRCSC      = copy_file.c
SUBDIRS    = test inodeserver

include top/mk/boilerplate.mk

CVSDB.o : copy_file.h
copy_file.o : copy_file.h