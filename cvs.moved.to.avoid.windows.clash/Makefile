LIB        = libcvs.a

SRCS       = CVSBasic.hs CVSHigh.hs CVSDB.hs
SRCSC      = copy_file.c
SUBDIRS    = test inodeserver

include top/mk/boilerplate.mk

CVSDB.o : copy_file.h
copy_file.o : copy_file.h