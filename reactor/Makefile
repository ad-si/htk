EXTRA_HC_OPTIONS += '-\#include "signaldispatcher.h"'  '-\#include "collector.h"'

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
# the following modules don't seem to be used anywhere and
# are now obsolete.
#	     Sensor.hs\
#	     NameSpace.hs\

SUBDIRS = test
      
SRCSH      = signaldispatcher.h collector.h

SRCSC      = signaldispatcher.c collector.c

EXTRA_HC_OPTIONS += -O -H30M

include top/mk/boilerplate.mk
