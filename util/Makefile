LIB        = libutil.a

SRCS       = Queue.hs ExtendedPrelude.hs Computation.hs Dynamics.hs WBFiles.hs Object.hs Date.hs Debug.hs Maybes.hs LineShow.hs Cache.hs FileNames.hs \
 IOExtras.hs QuickReadShow.hs AtomString.hs SmallSet.hs Registry.hs
SRCSC      = object.c

SUBDIRS = test

include top/mk/boilerplate.mk

object.c : object.h

