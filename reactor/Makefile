EXTRA_HC_OPTIONS += '-\#include "signaldispatcher.h"' 

LIB        = libreactor.a

SRCS       = Listener.hs\
	     ExternalEvent.hs\
	     Interaction.hs\
	     SIMClasses.hs\
	     EventStream.hs\
	     InterActor.hs\
	     Controller.hs\
	     EventLoop.hs\
	     WatchDog.hs\
	     ChildProcess.hs\
	     EventBroker.hs\
	     InfoBus.hs\
	     Observer.hs\
	     Interpreter.hs\
	     Dispatcher.hs\
	     Signal.hs\
	     Collector.hs\
             RegularExpression.hs\
	     Expect.hs\
	     SIM.hs 

SRCSH      = signaldispatcher.h
SRCSC      = signaldispatcher.c


SUBDIRS = test

EXTRA_HC_OPTIONS += -O -H30M

include top/mk/boilerplate.mk
